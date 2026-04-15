using RabbitMQ.AMQP.Client;
using RabbitMQ.AMQP.Client.Impl;

const string brokerUri = "amqp://guest:guest@localhost:5672/%2f";
const string taskQueueName = "task_queue";

ConnectionSettings settings = ConnectionSettingsBuilder.Create()
    .Uri(new Uri(brokerUri))
    .ContainerId("tutorial-worker")
    .Build();

IEnvironment environment = AmqpEnvironment.Create(settings);
IConnection connection = await environment.CreateConnectionAsync();

try
{
    IManagement management = connection.Management();
    IQueueSpecification queueSpec = management.Queue(taskQueueName).Type(QueueType.QUORUM);
    await queueSpec.DeclareAsync();

    IConsumer consumer = await connection.ConsumerBuilder()
        .Queue(taskQueueName)
        .InitialCredits(1)
        .MessageHandler((ctx, message) =>
        {
            string body = message.BodyAsString();
            Console.WriteLine($" [x] Received '{body}'");
            try
            {
                DoWork(body);
            }
            finally
            {
                Console.WriteLine(" [x] Done");
                ctx.Accept();
            }

            return Task.CompletedTask;
        })
        .BuildAndStartAsync();

    try
    {
        Console.WriteLine(" [*] Waiting for messages. To exit press CTRL+C");
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
        await consumer.CloseAsync();
    }
}
finally
{
    await connection.CloseAsync();
    await environment.CloseAsync();
}

static void DoWork(string task)
{
    int dots = task.ToCharArray().Count(ch => ch == '.');
    Thread.Sleep(dots * 1000);
}
