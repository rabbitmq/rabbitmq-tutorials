using RabbitMQ.AMQP.Client;
using RabbitMQ.AMQP.Client.Impl;

const string brokerUri = "amqp://guest:guest@localhost:5672/%2f";
const string exchangeName = "logs_direct";

if (args.Length < 1)
{
    Console.Error.WriteLine("Usage: ReceiveLogsDirect [info] [warning] [error]");
    Environment.Exit(1);
}

ConnectionSettings settings = ConnectionSettingsBuilder.Create()
    .Uri(new Uri(brokerUri))
    .ContainerId("tutorial-receivelogsdirect")
    .Build();

IEnvironment environment = AmqpEnvironment.Create(settings);
IConnection connection = await environment.CreateConnectionAsync();

try
{
    IManagement management = connection.Management();
    IExchangeSpecification exchangeSpec = management.Exchange(exchangeName).Type("direct");
    await exchangeSpec.DeclareAsync();

    IQueueSpecification tempQueue = management.Queue().Exclusive(true).AutoDelete(true);
    IQueueInfo queueInfo = await tempQueue.DeclareAsync();
    string queueName = queueInfo.Name();

    foreach (string severity in args)
    {
        IBindingSpecification binding = management.Binding()
            .SourceExchange(exchangeSpec)
            .DestinationQueue(queueName)
            .Key(severity);
        await binding.BindAsync();
    }

    IConsumer consumer = await connection.ConsumerBuilder()
        .Queue(queueName)
        .MessageHandler((ctx, message) =>
        {
            string body = message.BodyAsString();
            string routingKey = RoutingKey(message);
            Console.WriteLine($" [x] Received '{routingKey}':'{body}'");
            ctx.Accept();
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

static string RoutingKey(IMessage message)
{
    object? rk = message.Annotation("x-routing-key");
    if (rk != null)
    {
        return rk.ToString() ?? "";
    }

    return message.Subject() ?? "";
}
