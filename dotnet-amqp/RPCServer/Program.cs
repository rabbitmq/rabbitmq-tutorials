using System.Text;
using RabbitMQ.AMQP.Client;
using RabbitMQ.AMQP.Client.Impl;

const string brokerUri = "amqp://guest:guest@localhost:5672/%2f";
const string rpcQueueName = "rpc_queue";

ConnectionSettings settings = ConnectionSettingsBuilder.Create()
    .Uri(new Uri(brokerUri))
    .ContainerId("tutorial-rpcserver")
    .Build();

IEnvironment environment = AmqpEnvironment.Create(settings);
IConnection connection = await environment.CreateConnectionAsync();

try
{
    IManagement management = connection.Management();
    IQueueSpecification queueSpec = management.Queue(rpcQueueName).Type(QueueType.QUORUM);
    await queueSpec.DeclareAsync();
    await queueSpec.PurgeAsync();

    Console.WriteLine(" [x] Awaiting RPC requests");

    IResponder responder = await connection.ResponderBuilder()
        .RequestQueue(rpcQueueName)
        .Handler((ctx, request) =>
        {
            string response = "";
            try
            {
                string message = Encoding.UTF8.GetString(request.Body()!);
                int n = int.Parse(message);
                Console.WriteLine($" [.] fib({message})");
                response += Fib(n);
            }
            catch (Exception e)
            {
                Console.WriteLine($" [.] {e.Message}");
            }

            return Task.FromResult(ctx.Message(Encoding.UTF8.GetBytes(response)));
        })
        .BuildAsync();

    try
    {
        using var cts = new CancellationTokenSource();
        Console.CancelKeyPress += (_, e) =>
        {
            e.Cancel = true;
            cts.Cancel();
        };
        await Task.Delay(Timeout.Infinite, cts.Token);
    }
    catch (OperationCanceledException)
    {
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

static int Fib(int n)
{
    if (n == 0)
    {
        return 0;
    }

    if (n == 1)
    {
        return 1;
    }

    return Fib(n - 1) + Fib(n - 2);
}
