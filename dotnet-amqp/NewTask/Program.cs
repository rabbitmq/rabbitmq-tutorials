using System.Text;
using RabbitMQ.AMQP.Client;
using RabbitMQ.AMQP.Client.Impl;

const string brokerUri = "amqp://guest:guest@localhost:5672/%2f";
const string taskQueueName = "task_queue";

string message = args.Length > 0 ? string.Join(" ", args) : "Hello World!";

ConnectionSettings settings = ConnectionSettingsBuilder.Create()
    .Uri(new Uri(brokerUri))
    .ContainerId("tutorial-newtask")
    .Build();

IEnvironment environment = AmqpEnvironment.Create(settings);
IConnection connection = await environment.CreateConnectionAsync();

try
{
    IManagement management = connection.Management();
    IQueueSpecification queueSpec = management.Queue(taskQueueName).Type(QueueType.QUORUM);
    await queueSpec.DeclareAsync();

    IPublisher publisher = await connection.PublisherBuilder().Queue(taskQueueName).BuildAsync();
    try
    {
        var amqpMessage = new AmqpMessage(Encoding.UTF8.GetBytes(message));
        PublishResult pr = await publisher.PublishAsync(amqpMessage);
        if (pr.Outcome.State != OutcomeState.Accepted)
        {
            Console.Error.WriteLine($"Unexpected publish outcome: {pr.Outcome.State}");
            Environment.Exit(1);
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
