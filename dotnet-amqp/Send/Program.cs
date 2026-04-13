using System.Text;
using RabbitMQ.AMQP.Client;
using RabbitMQ.AMQP.Client.Impl;

const string brokerUri = "amqp://guest:guest@localhost:5672/%2f";

ConnectionSettings settings = ConnectionSettingsBuilder.Create()
    .Uri(new Uri(brokerUri))
    .ContainerId("tutorial-send")
    .Build();

IEnvironment environment = AmqpEnvironment.Create(settings);
IConnection connection = await environment.CreateConnectionAsync();

try
{
    IManagement management = connection.Management();
    IQueueSpecification queueSpec = management.Queue("hello").Type(QueueType.QUORUM);
    await queueSpec.DeclareAsync();

    IPublisher publisher = await connection.PublisherBuilder().Queue("hello").BuildAsync();
    try
    {
        const string body = "Hello World!";
        var message = new AmqpMessage(Encoding.UTF8.GetBytes(body));
        PublishResult pr = await publisher.PublishAsync(message);
        if (pr.Outcome.State != OutcomeState.Accepted)
        {
            Console.Error.WriteLine($"Unexpected publish outcome: {pr.Outcome.State}");
            Environment.Exit(1);
        }

        Console.WriteLine($" [x] Sent {body}");
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
