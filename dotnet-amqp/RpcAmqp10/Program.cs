using System.Text;
using RabbitMQ.AMQP.Client;
using RabbitMQ.AMQP.Client.Impl;

const string brokerUri = "amqp://guest:guest@localhost:5672/%2f";
const string requestQueue = "rpc-requests";

using var cts = new CancellationTokenSource();
Console.CancelKeyPress += (_, e) =>
{
    e.Cancel = true;
    cts.Cancel();
};

Console.WriteLine("Starting Direct Reply-To RPC example (RabbitMQ 4.2+ uses Direct Reply-To for the requester when no reply queue is set).");
Console.WriteLine("Ensure RabbitMQ is running on localhost:5672.");

Task serverTask = RunServerAsync(cts.Token);
await Task.Delay(500, CancellationToken.None);

Task clientTask = RunClientAsync(cts.Token);

try
{
    await Task.Delay(Timeout.Infinite, cts.Token);
}
catch (OperationCanceledException)
{
}

Console.WriteLine("Application shutting down...");
await Task.WhenAll(
    AwaitCancelled(serverTask),
    AwaitCancelled(clientTask));

static async Task AwaitCancelled(Task task)
{
    try
    {
        await task;
    }
    catch (OperationCanceledException)
    {
    }
}

async Task RunServerAsync(CancellationToken cancellationToken)
{
    ConnectionSettings settings = ConnectionSettingsBuilder.Create()
        .Uri(new Uri(brokerUri))
        .ContainerId("rpc-amqp10-server")
        .Build();

    IEnvironment environment = AmqpEnvironment.Create(settings);
    IConnection connection = await environment.CreateConnectionAsync();

    try
    {
        IManagement management = connection.Management();
        IQueueSpecification queueSpec = management.Queue(requestQueue).Type(QueueType.QUORUM);
        await queueSpec.DeclareAsync();

        IResponder responder = await connection.ResponderBuilder()
            .RequestQueue(requestQueue)
            .Handler((ctx, request) =>
            {
                string payload = Encoding.UTF8.GetString(request.Body()!);
                object? mid = request.MessageId();
                if (mid != null)
                {
                    Console.WriteLine($"RPC Server: Received {payload} request (ID: {mid})");
                }
                else
                {
                    Console.WriteLine($"RPC Server: Received {payload} request");
                }

                return Task.FromResult(ctx.Message(Encoding.UTF8.GetBytes("pong")));
            })
            .BuildAsync();

        try
        {
            Console.WriteLine("RPC Server: Started and listening for requests...");
            await Task.Delay(Timeout.Infinite, cancellationToken);
        }
        catch (OperationCanceledException)
        {
            Console.WriteLine("RPC Server: Shutting down...");
        }
        finally
        {
            await responder.CloseAsync();
        }
    }
    finally
    {
        await connection.CloseAsync();
        await environment.CloseAsync();
    }
}

async Task RunClientAsync(CancellationToken cancellationToken)
{
    ConnectionSettings settings = ConnectionSettingsBuilder.Create()
        .Uri(new Uri(brokerUri))
        .ContainerId("rpc-amqp10-client")
        .Build();

    IEnvironment environment = AmqpEnvironment.Create(settings);
    IConnection connection = await environment.CreateConnectionAsync();

    try
    {
        IRequester requester = await connection.RequesterBuilder()
            .RequestAddress()
            .Queue(requestQueue)
            .Requester()
            .BuildAsync();

        try
        {
            Console.WriteLine($"RPC Client: Reply address: {requester.GetReplyToQueue()}");
            Console.WriteLine("RPC Client: Started. Sending ping every second (CTRL+C to exit).");

            int requestId = 0;
            using var ticker = new PeriodicTimer(TimeSpan.FromSeconds(1));
            while (await ticker.WaitForNextTickAsync(cancellationToken))
            {
                requestId++;
                IMessage reply;
                try
                {
                    reply = await requester.PublishAsync(new AmqpMessage(Encoding.UTF8.GetBytes("ping")),
                        cancellationToken);
                }
                catch (Exception ex)
                {
                    Console.WriteLine($"RPC Client: Error sending request: {ex.Message}");
                    continue;
                }

                string payload = Encoding.UTF8.GetString(reply.Body()!);
                Console.WriteLine($"RPC Client: Sent ping request ({requestId})");
                Console.WriteLine($"RPC Client: Received reply - {payload}");
            }
        }
        catch (OperationCanceledException)
        {
            Console.WriteLine("RPC Client: Shutting down...");
        }
        finally
        {
            await requester.CloseAsync();
        }
    }
    finally
    {
        await connection.CloseAsync();
        await environment.CloseAsync();
    }
}
