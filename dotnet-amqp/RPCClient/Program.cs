using System.Text;
using RabbitMQ.AMQP.Client;
using RabbitMQ.AMQP.Client.Impl;

const string brokerUri = "amqp://guest:guest@localhost:5672/%2f";

ConnectionSettings settings = ConnectionSettingsBuilder.Create()
    .Uri(new Uri(brokerUri))
    .ContainerId("tutorial-rpcclient")
    .Build();

IEnvironment environment = AmqpEnvironment.Create(settings);
IConnection connection = await environment.CreateConnectionAsync();

try
{
    IRequester requester = await connection.RequesterBuilder()
        .RequestAddress()
        .Queue("rpc_queue")
        .Requester()
        .BuildAsync();

    try
    {
        for (int i = 0; i < 32; i++)
        {
            string iStr = i.ToString();
            Console.WriteLine($" [x] Requesting fib({iStr})");
            IMessage request = new AmqpMessage(Encoding.UTF8.GetBytes(iStr));
            IMessage reply = await requester.PublishAsync(request);
            string response = Encoding.UTF8.GetString(reply.Body()!);
            Console.WriteLine($" [.] Got '{response}'");
        }
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
