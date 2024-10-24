using RabbitMQ.Client;
using RabbitMQ.Client.Exceptions;
using System.Diagnostics;
using System.Text;

const int MessageCount = 50_000;
const int MaxOutstandingConfirms = 128;

var channelOptions = new CreateChannelOptions
{
    PublisherConfirmationsEnabled = true,
    PublisherConfirmationTrackingEnabled = true,
    OutstandingPublisherConfirmationsRateLimiter = new ThrottlingRateLimiter(MaxOutstandingConfirms)
};

var props = new BasicProperties
{
    Persistent = true
};

await PublishMessagesIndividuallyAsync();
await PublishMessagesInBatchAsync();
await HandlePublishConfirmsAsynchronously();

static Task<IConnection> CreateConnectionAsync()
{
    var factory = new ConnectionFactory { HostName = "localhost" };
    return factory.CreateConnectionAsync();
}

async Task PublishMessagesIndividuallyAsync()
{
    Console.WriteLine($"{DateTime.Now} [INFO] publishing {MessageCount:N0} messages individually " +
                       "and handling confirms individually (i.e., the slowest way)");

    using IConnection connection = await CreateConnectionAsync();
    using IChannel channel = await connection.CreateChannelAsync(channelOptions);

    // declare a server-named queue
    QueueDeclareOk queueDeclareResult = await channel.QueueDeclareAsync();
    string queueName = queueDeclareResult.QueueName;

    var sw = new Stopwatch();
    sw.Start();

    for (int i = 0; i < MessageCount; i++)
    {
        byte[] body = Encoding.UTF8.GetBytes(i.ToString());
        try
        {
            await channel.BasicPublishAsync(exchange: string.Empty, routingKey: queueName, body: body,
                mandatory: true, basicProperties: props);
        }
        catch (PublishException pubEx)
        {
            Console.Error.WriteLine("{0} [ERROR] publish exception: {1}", DateTime.Now, pubEx);
        }
        catch (Exception ex)
        {
            Console.Error.WriteLine("{0} [ERROR] other exception: {1}", DateTime.Now, ex);
        }
    }

    sw.Stop();

    Console.WriteLine($"{DateTime.Now} [INFO] published {MessageCount:N0} messages individually " +
                      $"in {sw.ElapsedMilliseconds:N0} ms");
}

async Task PublishMessagesInBatchAsync()
{
    Console.WriteLine($"{DateTime.Now} [INFO] publishing {MessageCount:N0} messages and handling " +
                      $"confirms in batches");

    using IConnection connection = await CreateConnectionAsync();
    using IChannel channel = await connection.CreateChannelAsync(channelOptions);

    // declare a server-named queue
    QueueDeclareOk queueDeclareResult = await channel.QueueDeclareAsync();
    string queueName = queueDeclareResult.QueueName;

    /*
     * Note: since throttling happens when 50% of the outstanding confirms are reached,
     * each batch size should not be greater than this value
     */
    int batchSize = MaxOutstandingConfirms / 2;
    int outstandingMessageCount = 0;

    var sw = new Stopwatch();
    sw.Start();

    static async Task AwaitPublishTasks(IEnumerable<ValueTask> publishTasks)
    {
        foreach (ValueTask pt in publishTasks)
        {
            try
            {
                await pt;
            }
            catch (PublishException pubEx)
            {
                Console.Error.WriteLine("{0} [ERROR] publish exception: {1}", DateTime.Now, pubEx);
            }
            catch (Exception ex)
            {
                Console.Error.WriteLine("{0} [ERROR] other exception: {1}", DateTime.Now, ex);
            }
        }
    }

    var publishTasks = new List<ValueTask>();
    for (int i = 0; i < MessageCount; i++)
    {
        byte[] body = Encoding.UTF8.GetBytes(i.ToString());

        var pt0 = channel.BasicPublishAsync(exchange: string.Empty, routingKey: queueName, body: body,
            mandatory: true, basicProperties: props);
        publishTasks.Add(pt0);

        outstandingMessageCount++;

        if (outstandingMessageCount == batchSize)
        {
            await AwaitPublishTasks(publishTasks);
            publishTasks.Clear();
            outstandingMessageCount = 0;
        }
    }

    if (publishTasks.Count > 0)
    {
        await AwaitPublishTasks(publishTasks);
    }

    sw.Stop();
    Console.WriteLine($"{DateTime.Now} [INFO] published {MessageCount:N0} messages in batch in " +
                      $"{sw.ElapsedMilliseconds:N0} ms");
}

async Task HandlePublishConfirmsAsynchronously()
{
    Console.WriteLine($"{DateTime.Now} [INFO] publishing {MessageCount:N0} messages and " +
                      $"handling confirms asynchronously");

    // NOTE: setting trackConfirmations to false because this program
    // is tracking them itself.
    channelOptions.PublisherConfirmationTrackingEnabled = false;
    channelOptions.OutstandingPublisherConfirmationsRateLimiter = null;

    using IConnection connection = await CreateConnectionAsync();
    using IChannel channel = await connection.CreateChannelAsync(channelOptions);

    // declare a server-named queue
    QueueDeclareOk queueDeclareResult = await channel.QueueDeclareAsync();
    string queueName = queueDeclareResult.QueueName;

    bool publishingCompleted = false;
    var allMessagesConfirmedTcs =
        new TaskCompletionSource<bool>(TaskCreationOptions.RunContinuationsAsynchronously);
    var outstandingConfirms = new LinkedList<ulong>();
    var semaphore = new SemaphoreSlim(1, 1);
    async Task CleanOutstandingConfirms(ulong deliveryTag, bool multiple)
    {
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

    channel.BasicAcksAsync += (sender, ea) =>
        CleanOutstandingConfirms(ea.DeliveryTag, ea.Multiple);

    channel.BasicNacksAsync += (sender, ea) =>
    {
        Console.WriteLine($"{DateTime.Now} [WARNING] message sequence number: {ea.DeliveryTag} " +
                          $"has been nacked (multiple: {ea.Multiple})");
        return CleanOutstandingConfirms(ea.DeliveryTag, ea.Multiple);
    };

    var sw = new Stopwatch();
    sw.Start();

    var publishTasks = new List<ValueTask>();
    for (int i = 0; i < MessageCount; i++)
    {
        string msg = i.ToString();
        byte[] body = Encoding.UTF8.GetBytes(msg);
        ulong nextPublishSeqNo = await channel.GetNextPublishSequenceNumberAsync();
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
    Console.WriteLine($"{DateTime.Now} [INFO] published {MessageCount:N0} messages and handled " +
                      $"confirm asynchronously {sw.ElapsedMilliseconds:N0} ms");
}
