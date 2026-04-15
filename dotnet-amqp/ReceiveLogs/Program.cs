using RabbitMQ.AMQP.Client;
using RabbitMQ.AMQP.Client.Impl;

const string brokerUri = "amqp://guest:guest@localhost:5672/%2f";
const string exchangeName = "logs";

ConnectionSettings settings = ConnectionSettingsBuilder.Create()
    .Uri(new Uri(brokerUri))
    .ContainerId("tutorial-receivelogs")
    .Build();

IEnvironment environment = AmqpEnvironment.Create(settings);
IConnection connection = await environment.CreateConnectionAsync();

try
{
    IManagement management = connection.Management();
    IExchangeSpecification exchangeSpec = management.Exchange(exchangeName).Type("fanout");
    await exchangeSpec.DeclareAsync();

    IQueueSpecification tempQueue = management.Queue().Exclusive(true).AutoDelete(true);
    IQueueInfo queueInfo = await tempQueue.DeclareAsync();
    string queueName = queueInfo.Name();

    IBindingSpecification binding = management.Binding()
        .SourceExchange(exchangeSpec)
        .DestinationQueue(queueName)
        .Key(string.Empty);
    await binding.BindAsync();

    IConsumer consumer = await connection.ConsumerBuilder()
        .Queue(queueName)
        .MessageHandler((ctx, message) =>
        {
            string body = message.BodyAsString();
            Console.WriteLine($" [x] Received '{body}'");
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
