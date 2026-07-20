using RabbitMQ.Client;
using RabbitMQ.Client.Events;
using System.Text;

const string QUEUE_NAME = "rpc_queue";

var factory = new ConnectionFactory { HostName = "localhost" };
using var connection = await factory.CreateConnectionAsync();
using var channel = await connection.CreateChannelAsync();

await channel.QueueDeclareAsync(queue: QUEUE_NAME, durable: true, exclusive: false,
    autoDelete: false, arguments: new Dictionary<string, object?> { { "x-queue-type", "quorum" } });

await channel.BasicQosAsync(prefetchSize: 0, prefetchCount: 1, global: false);

var consumer = new AsyncEventingBasicConsumer(channel);
consumer.ReceivedAsync += async (object sender, BasicDeliverEventArgs ea) =>
{
    AsyncEventingBasicConsumer cons = (AsyncEventingBasicConsumer)sender;
    IChannel ch = cons.Channel;
    string response = string.Empty;

    byte[] body = ea.Body.ToArray();
    IReadOnlyBasicProperties props = ea.BasicProperties;
    var replyProps = new BasicProperties
    {
        CorrelationId = props.CorrelationId
    };

    try
    {
        var message = Encoding.UTF8.GetString(body);
        int n = int.Parse(message);
        Console.WriteLine($" [.] Fib({message})");
        response = Fib(n).ToString();
    }
    catch (Exception e)
    {
        Console.WriteLine($" [.] {e.Message}");
        response = string.Empty;
    }
    finally
    {
        var responseBytes = Encoding.UTF8.GetBytes(response);
        await ch.BasicPublishAsync(exchange: string.Empty, routingKey: props.ReplyTo!,
            mandatory: true, basicProperties: replyProps, body: responseBytes);
        await ch.BasicAckAsync(deliveryTag: ea.DeliveryTag, multiple: false);
    }
};

await channel.BasicConsumeAsync(QUEUE_NAME, false, consumer);
Console.WriteLine(" [x] Awaiting RPC requests");
Console.WriteLine(" Press [enter] to exit.");
Console.ReadLine();

// Assumes only valid positive integer input.
static int Fib(int n)
{
    int a = 0;
    int b = 1;

    for (int i = 0; i < n; i++)
    {
        (a, b) = (b, a + b);
    }

    return a;
}
