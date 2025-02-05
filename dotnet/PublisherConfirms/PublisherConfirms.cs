using RabbitMQ.Client;
using System.Buffers.Binary;
using System.Diagnostics;
using System.Text;

const ushort MAX_OUTSTANDING_CONFIRMS = 256;

const int MESSAGE_COUNT = 50_000;
bool debug = false;

var channelOpts = new CreateChannelOptions(
    publisherConfirmationsEnabled: true,
    publisherConfirmationTrackingEnabled: true,
    outstandingPublisherConfirmationsRateLimiter: new ThrottlingRateLimiter(MAX_OUTSTANDING_CONFIRMS)
);

var props = new BasicProperties
{
    Persistent = true
};

string hostname = "localhost";
if (args.Length > 0)
{
    if (false == string.IsNullOrWhiteSpace(args[0]))
    {
        hostname = args[0];
    }
}

#pragma warning disable CS8321 // Local function is declared but never used

await PublishMessagesIndividuallyAsync();
await PublishMessagesInBatchAsync();
await HandlePublishConfirmsAsynchronously();

Task<IConnection> CreateConnectionAsync()
{
    var factory = new ConnectionFactory { HostName = hostname };
    return factory.CreateConnectionAsync();
}

async Task PublishMessagesIndividuallyAsync()
{
    Console.WriteLine($"{DateTime.Now} [INFO] publishing {MESSAGE_COUNT:N0} messages and handling confirms per-message");

    await using IConnection connection = await CreateConnectionAsync();
    await using IChannel channel = await connection.CreateChannelAsync(channelOpts);

    // declare a server-named queue
    QueueDeclareOk queueDeclareResult = await channel.QueueDeclareAsync();
    string queueName = queueDeclareResult.QueueName;

    var sw = new Stopwatch();
    sw.Start();

    for (int i = 0; i < MESSAGE_COUNT; i++)
    {
        byte[] body = Encoding.UTF8.GetBytes(i.ToString());
        try
        {
            await channel.BasicPublishAsync(exchange: string.Empty, routingKey: queueName, body: body, basicProperties: props, mandatory: true);
        }
        catch (Exception ex)
        {
            Console.Error.WriteLine($"{DateTime.Now} [ERROR] saw nack or return, ex: {ex}");
        }
    }

    sw.Stop();

    Console.WriteLine($"{DateTime.Now} [INFO] published {MESSAGE_COUNT:N0} messages individually in {sw.ElapsedMilliseconds:N0} ms");
}

async Task PublishMessagesInBatchAsync()
{
    Console.WriteLine($"{DateTime.Now} [INFO] publishing {MESSAGE_COUNT:N0} messages and handling confirms in batches");

    await using IConnection connection = await CreateConnectionAsync();
    await using IChannel channel = await connection.CreateChannelAsync(channelOpts);

    // declare a server-named queue
    QueueDeclareOk queueDeclareResult = await channel.QueueDeclareAsync();
    string queueName = queueDeclareResult.QueueName;

    int batchSize = Math.Max(1, MAX_OUTSTANDING_CONFIRMS / 2);

    var sw = Stopwatch.StartNew();

    var publishTasks = new List<ValueTask>();
    for (int i = 0; i < MESSAGE_COUNT; i++)
    {
        byte[] body = Encoding.UTF8.GetBytes(i.ToString());
        ValueTask publishTask = channel.BasicPublishAsync(exchange: string.Empty, routingKey: queueName, body: body, mandatory: true, basicProperties: props);
        publishTasks.Add(publishTask);

        await MaybeAwaitPublishes(publishTasks, batchSize);
    }

    // Await any remaining tasks in case message count was not
    // evenly divisible by batch size.
    await MaybeAwaitPublishes(publishTasks, 0);

    sw.Stop();
    Console.WriteLine($"{DateTime.Now} [INFO] published {MESSAGE_COUNT:N0} messages in batch in {sw.ElapsedMilliseconds:N0} ms");
}

static async Task MaybeAwaitPublishes(List<ValueTask> publishTasks, int batchSize)
{
    if (publishTasks.Count >= batchSize)
    {
        foreach (ValueTask pt in publishTasks)
        {
            try
            {
                await pt;
            }
            catch (Exception ex)
            {
                Console.Error.WriteLine($"{DateTime.Now} [ERROR] saw nack or return, ex: '{ex}'");
            }
        }
        publishTasks.Clear();
    }
}

async Task HandlePublishConfirmsAsynchronously()
{
    Console.WriteLine($"{DateTime.Now} [INFO] publishing {MESSAGE_COUNT:N0} messages and handling confirms asynchronously");

    await using IConnection connection = await CreateConnectionAsync();

    channelOpts = new CreateChannelOptions(publisherConfirmationsEnabled: true, publisherConfirmationTrackingEnabled: false);
    await using IChannel channel = await connection.CreateChannelAsync(channelOpts);

    // declare a server-named queue
    QueueDeclareOk queueDeclareResult = await channel.QueueDeclareAsync();
    string queueName = queueDeclareResult.QueueName;

    var allMessagesConfirmedTcs = new TaskCompletionSource<bool>(TaskCreationOptions.RunContinuationsAsynchronously);
    var outstandingConfirms = new LinkedList<ulong>();
    var semaphore = new SemaphoreSlim(1, 1);
    int confirmedCount = 0;
    async Task CleanOutstandingConfirms(ulong deliveryTag, bool multiple)
    {
        if (debug)
        {
            Console.WriteLine("{0} [DEBUG] confirming message: {1} (multiple: {2})",
                DateTime.Now, deliveryTag, multiple);
        }

        await semaphore.WaitAsync();
        try
        {
            if (multiple)
            {
                do
                {
                    LinkedListNode<ulong>? node = outstandingConfirms.First;
                    if (node is null)
                    {
                        break;
                    }
                    if (node.Value <= deliveryTag)
                    {
                        outstandingConfirms.RemoveFirst();
                    }
                    else
                    {
                        break;
                    }

                    confirmedCount++;
                } while (true);
            }
            else
            {
                confirmedCount++;
                outstandingConfirms.Remove(deliveryTag);
            }
        }
        finally
        {
            semaphore.Release();
        }

        if (outstandingConfirms.Count == 0 || confirmedCount == MESSAGE_COUNT)
        {
            allMessagesConfirmedTcs.SetResult(true);
        }
    }

    channel.BasicReturnAsync += (sender, ea) =>
    {
        ulong sequenceNumber = 0;

        IReadOnlyBasicProperties props = ea.BasicProperties;
        if (props.Headers is not null)
        {
            object? maybeSeqNum = props.Headers[Constants.PublishSequenceNumberHeader];
            if (maybeSeqNum is not null)
            {
                sequenceNumber = BinaryPrimitives.ReadUInt64BigEndian((byte[])maybeSeqNum);
            }
        }

        Console.WriteLine($"{DateTime.Now} [WARNING] message sequence number {sequenceNumber} has been basic.return-ed");
        return CleanOutstandingConfirms(sequenceNumber, false);
    };

    channel.BasicAcksAsync += (sender, ea) => CleanOutstandingConfirms(ea.DeliveryTag, ea.Multiple);
    channel.BasicNacksAsync += (sender, ea) =>
    {
        Console.WriteLine($"{DateTime.Now} [WARNING] message sequence number: {ea.DeliveryTag} has been nacked (multiple: {ea.Multiple})");
        return CleanOutstandingConfirms(ea.DeliveryTag, ea.Multiple);
    };

    var sw = new Stopwatch();
    sw.Start();

    var publishTasks = new List<ValueTuple<ulong, ValueTask>>();
    for (int i = 0; i < MESSAGE_COUNT; i++)
    {
        string msg = i.ToString();
        byte[] body = Encoding.UTF8.GetBytes(msg);
        ulong nextPublishSeqNo = await channel.GetNextPublishSequenceNumberAsync();
        if ((ulong)(i + 1) != nextPublishSeqNo)
        {
            Console.WriteLine($"{DateTime.Now} [WARNING] i {i + 1} does not equal next sequence number: {nextPublishSeqNo}");
        }
        await semaphore.WaitAsync();
        try
        {
            outstandingConfirms.AddLast(nextPublishSeqNo);
        }
        finally
        {
            semaphore.Release();
        }

        string rk = queueName;
        if (i % 1000 == 0)
        {
            // This will cause a basic.return, for fun
            rk = Guid.NewGuid().ToString();
        }
        (ulong, ValueTask) data =
            (nextPublishSeqNo, channel.BasicPublishAsync(exchange: string.Empty, routingKey: rk, body: body, mandatory: true, basicProperties: props));
        publishTasks.Add(data);
    }

    using var cts = new CancellationTokenSource(TimeSpan.FromSeconds(10));
    // await Task.WhenAll(publishTasks).WaitAsync(cts.Token);
    foreach ((ulong SeqNo, ValueTask PublishTask) datum in publishTasks)
    {
        try
        {
            await datum.PublishTask;
        }
        catch (Exception ex)
        {
            Console.Error.WriteLine($"{DateTime.Now} [ERROR] saw nack, seqNo: '{datum.SeqNo}', ex: '{ex}'");
        }
    }

    try
    {
        await allMessagesConfirmedTcs.Task.WaitAsync(cts.Token);
    }
    catch (OperationCanceledException)
    {
        Console.Error.WriteLine("{0} [ERROR] all messages could not be published and confirmed within 10 seconds", DateTime.Now);
    }
    catch (TimeoutException)
    {
        Console.Error.WriteLine("{0} [ERROR] all messages could not be published and confirmed within 10 seconds", DateTime.Now);
    }

    sw.Stop();
    Console.WriteLine($"{DateTime.Now} [INFO] published {MESSAGE_COUNT:N0} messages and handled confirm asynchronously {sw.ElapsedMilliseconds:N0} ms");
}
