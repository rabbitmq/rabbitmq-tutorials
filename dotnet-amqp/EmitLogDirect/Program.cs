using System.Text;
using RabbitMQ.AMQP.Client;
using RabbitMQ.AMQP.Client.Impl;

const string brokerUri = "amqp://guest:guest@localhost:5672/%2f";
const string exchangeName = "logs_direct";

string severity = GetSeverity(args);
string message = GetMessage(args);

ConnectionSettings settings = ConnectionSettingsBuilder.Create()
    .Uri(new Uri(brokerUri))
    .ContainerId("tutorial-emitlogdirect")
    .Build();

IEnvironment environment = AmqpEnvironment.Create(settings);
IConnection connection = await environment.CreateConnectionAsync();

try
{
    IManagement management = connection.Management();
    IExchangeSpecification exchangeSpec = management.Exchange(exchangeName).Type("direct");
    await exchangeSpec.DeclareAsync();

    IPublisher publisher = await connection.PublisherBuilder().Exchange(exchangeName).Key(severity).BuildAsync();
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

        Console.WriteLine($" [x] Sent '{severity}':'{message}'");
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

static string GetSeverity(string[] strings) => strings.Length < 1 ? "info" : strings[0];

static string GetMessage(string[] strings)
{
    if (strings.Length < 2)
    {
        return "Hello World!";
    }

    return string.Join(" ", strings.Skip(1));
}
