using RabbitMQ.Client;
using System.Diagnostics;
using System.Text;

const int MESSAGE_COUNT = 50_000;

await PublishMessagesIndividuallyAsync();
await PublishMessagesInBatchAsync();
await HandlePublishConfirmsAsynchronously();

static Task<IConnection> CreateConnectionAsync()
{
    var factory = new ConnectionFactory { HostName = "localhost" };
    return factory.CreateConnectionAsync();
}

static async Task PublishMessagesIndividuallyAsync()
{
    Console.WriteLine($"{DateTime.Now} [INFO] publishing {MESSAGE_COUNT:N0} messages individually " +
                       "and handling confirms all at once");

    using IConnection connection = await CreateConnectionAsync();
    using IChannel channel = await connection.CreateChannelAsync();

    // declare a server-named queue
    QueueDeclareOk queueDeclareResult = await channel.QueueDeclareAsync();
    string queueName = queueDeclareResult.QueueName;
    await channel.ConfirmSelectAsync();

    var sw = new Stopwatch();
    sw.Start();

    for (int i = 0; i < MESSAGE_COUNT; i++)
    {
        byte[] body = Encoding.UTF8.GetBytes(i.ToString());
        await channel.BasicPublishAsync(exchange: string.Empty, routingKey: queueName, body: body);
    }

    await channel.WaitForConfirmsOrDieAsync();

    sw.Stop();

    Console.WriteLine($"{DateTime.Now} [INFO] published {MESSAGE_COUNT:N0} messages individually " +
                      $"in {sw.ElapsedMilliseconds:N0} ms");
}

static async Task PublishMessagesInBatchAsync()
{
    Console.WriteLine($"{DateTime.Now} [INFO] publishing {MESSAGE_COUNT:N0} messages and handling " +
                      $"confirms in batches");

    using IConnection connection = await CreateConnectionAsync();
    using IChannel channel = await connection.CreateChannelAsync();

    // declare a server-named queue
    QueueDeclareOk queueDeclareResult = await channel.QueueDeclareAsync();
    string queueName = queueDeclareResult.QueueName;
    await channel.ConfirmSelectAsync();

    int batchSize = 100;
    int outstandingMessageCount = 0;

    var sw = new Stopwatch();
    sw.Start();

    var publishTasks = new List<Task>();
    for (int i = 0; i < MESSAGE_COUNT; i++)
    {
        byte[] body = Encoding.UTF8.GetBytes(i.ToString());
        var pt = channel.BasicPublishAsync(exchange: string.Empty,
                                           routingKey: queueName, body: body);
        publishTasks.Add(pt.AsTask());
        outstandingMessageCount++;

        if (outstandingMessageCount == batchSize)
        {
            using var cts = new CancellationTokenSource(TimeSpan.FromSeconds(5));
            await Task.WhenAll(publishTasks).WaitAsync(cts.Token);
            publishTasks.Clear();

            await channel.WaitForConfirmsOrDieAsync(cts.Token);
            outstandingMessageCount = 0;
        }
    }

    if (outstandingMessageCount > 0)
    {
        using var cts = new CancellationTokenSource(TimeSpan.FromSeconds(5));
        await channel.WaitForConfirmsOrDieAsync(cts.Token);
    }

    sw.Stop();
    Console.WriteLine($"{DateTime.Now} [INFO] published {MESSAGE_COUNT:N0} messages in batch in " +
                      $"{sw.ElapsedMilliseconds:N0} ms");
}

async Task HandlePublishConfirmsAsynchronously()
{
    Console.WriteLine($"{DateTime.Now} [INFO] publishing {MESSAGE_COUNT:N0} messages and " +
                      $"handling confirms asynchronously");

    using IConnection connection = await CreateConnectionAsync();
    using IChannel channel = await connection.CreateChannelAsync();

    // declare a server-named queue
    QueueDeclareOk queueDeclareResult = await channel.QueueDeclareAsync();
    string queueName = queueDeclareResult.QueueName;

    // NOTE: setting trackConfirmations to false because this program
    // is tracking them itself.
    await channel.ConfirmSelectAsync(trackConfirmations: false);

    bool publishingCompleted = false;
    var allMessagesConfirmedTcs =
        new TaskCompletionSource<bool>(TaskCreationOptions.RunContinuationsAsynchronously);
    var outstandingConfirms = new LinkedList<ulong>();
    var semaphore = new SemaphoreSlim(1, 1);
    void CleanOutstandingConfirms(ulong deliveryTag, bool multiple)
    {
        semaphore.Wait();
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
                } while (true);
            }
            else
            {
                outstandingConfirms.Remove(deliveryTag);
            }
        }
        finally
        {
            semaphore.Release();
        }

        if (publishingCompleted && outstandingConfirms.Count == 0)
        {
            allMessagesConfirmedTcs.SetResult(true);
        }
    }

    channel.BasicAcks += (sender, ea) => CleanOutstandingConfirms(ea.DeliveryTag, ea.Multiple);
    channel.BasicNacks += (sender, ea) =>
    {
        Console.WriteLine($"{DateTime.Now} [WARNING] message sequence number: {ea.DeliveryTag} " +
                          $"has been nacked (multiple: {ea.Multiple})");
        CleanOutstandingConfirms(ea.DeliveryTag, ea.Multiple);
    };

    var sw = new Stopwatch();
    sw.Start();

    var publishTasks = new List<ValueTask>();
    for (int i = 0; i < MESSAGE_COUNT; i++)
    {
        string msg = i.ToString();
        byte[] body = Encoding.UTF8.GetBytes(msg);
        ulong nextPublishSeqNo = channel.NextPublishSeqNo;
        if ((ulong)(i + 1) != nextPublishSeqNo)
        {
            Console.WriteLine($"{DateTime.Now} [WARNING] i {i + 1} does not equal next sequence " +
                              $"number: {nextPublishSeqNo}");
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
        var pt = channel.BasicPublishAsync(exchange: string.Empty,
                                           routingKey: queueName, body: body);
        publishTasks.Add(pt);
    }

    using var cts = new CancellationTokenSource(TimeSpan.FromSeconds(10));

    try
    {
        foreach (ValueTask pt in publishTasks)
        {
            await pt;
            cts.Token.ThrowIfCancellationRequested();
        }
        publishingCompleted = true;
        await allMessagesConfirmedTcs.Task.WaitAsync(cts.Token);
    }
    catch (OperationCanceledException)
    {
        Console.Error.WriteLine("{0} [ERROR] all messages could not be published and confirmed " +
                                "within 10 seconds", DateTime.Now);
    }
    catch (TimeoutException)
    {
        Console.Error.WriteLine("{0} [ERROR] all messages could not be published and confirmed " +
                                "within 10 seconds", DateTime.Now);
    }

    sw.Stop();
    Console.WriteLine($"{DateTime.Now} [INFO] published {MESSAGE_COUNT:N0} messages and handled " +
                      $"confirm asynchronously {sw.ElapsedMilliseconds:N0} ms");
}
