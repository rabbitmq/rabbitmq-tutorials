using System.Text;
using RabbitMQ.AMQP.Client;
using RabbitMQ.AMQP.Client.Impl;

const string brokerUri = "amqp://guest:guest@localhost:5672/%2f";

ConnectionSettings settings = ConnectionSettingsBuilder.Create()
    .Uri(new Uri(brokerUri))
    .ContainerId("tutorial-publisherconfirms")
    .Build();

IEnvironment environment = AmqpEnvironment.Create(settings);
IConnection connection = await environment.CreateConnectionAsync();

try
{
    IManagement management = connection.Management();
    string queueName = Guid.NewGuid().ToString();
    IQueueSpecification queueSpec = management.Queue(queueName).Exclusive(true).AutoDelete(true);
    await queueSpec.DeclareAsync();

    IConsumer consumer = await connection.ConsumerBuilder()
        .Queue(queueName)
        .MessageHandler((ctx, message) =>
        {
            Console.WriteLine($"Received a message: {message.BodyAsString()}");
            ctx.Accept();
            return Task.CompletedTask;
        })
        .BuildAndStartAsync();

    try
    {
        await Task.Delay(300);

        IPublisher publisher = await connection.PublisherBuilder().Queue(queueName).BuildAsync();
        try
        {
            const string text = "hello";
            PublishResult pr = await publisher.PublishAsync(new AmqpMessage(Encoding.UTF8.GetBytes(text)));
            switch (pr.Outcome.State) {
            case OutcomeState.Accepted:
              Console.WriteLine($" Accepted Message: {pr.Message.BodyAsString()} confirmed");
              break;
            case OutcomeState.Released: // here the message is not routed
              Console.WriteLine($" Released Message: {pr.Message.BodyAsString()} Released");
              break;
            case OutcomeState.Rejected: // here there is also the error: `pr.Outcome.Error` 
              Console.WriteLine($"[Publisher] Message: {pr.Message.BodyAsString()} Rejected with error: {pr.Outcome.Error}");
              break;
            }

            await Task.Delay(500);
        }
        finally
        {
            await publisher.CloseAsync();
        }
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
