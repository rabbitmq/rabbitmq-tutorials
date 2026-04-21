using System.Text;
using RabbitMQ.AMQP.Client;
using RabbitMQ.AMQP.Client.Impl;

const string brokerUri = "amqp://guest:guest@localhost:5672/%2f";
const string exchangeName = "logs";

string message = args.Length < 1 ? "info: Hello World!" : string.Join(" ", args);

ConnectionSettings settings = ConnectionSettingsBuilder.Create()
    .Uri(new Uri(brokerUri))
    .ContainerId("tutorial-emitlog")
    .Build();

IEnvironment environment = AmqpEnvironment.Create(settings);
IConnection connection = await environment.CreateConnectionAsync();

try
{
    IManagement management = connection.Management();
    IExchangeSpecification exchangeSpec = management.Exchange(exchangeName).Type("fanout");
    await exchangeSpec.DeclareAsync();

    IPublisher publisher = await connection.PublisherBuilder().Exchange(exchangeName).BuildAsync();
    try
    {
        var amqpMessage = new AmqpMessage(Encoding.UTF8.GetBytes(message));
        PublishResult pr = await publisher.PublishAsync(amqpMessage);
        switch (pr.Outcome.State)
        {
        case OutcomeState.Accepted:
            break;
        case OutcomeState.Released:
            Console.Error.WriteLine($"Released message: {pr.Message.BodyAsString()}");
            Environment.Exit(1);
            break;
        case OutcomeState.Rejected:
            Console.Error.WriteLine($"[Publisher] Message: {pr.Message.BodyAsString()} rejected with error: {pr.Outcome.Error}");
            Environment.Exit(1);
            break;
        default:
            Console.Error.WriteLine($"Unexpected publish outcome: {pr.Outcome.State}");
            Environment.Exit(1);
            break;
        }

        Console.WriteLine($" [x] Sent '{message}'");
    }
    finally
    {
        await publisher.CloseAsync();
    }
}
finally
{
    await connection.CloseAsync();
    await environment.CloseAsync();
}
