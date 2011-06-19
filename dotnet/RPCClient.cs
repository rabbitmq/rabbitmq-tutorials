using System;
using RabbitMQ.Client;
using RabbitMQ.Client.Events;

class RPCClient : IDisposable {
    private IConnection connection;
    private IModel channel;
    private string replyQueueName;
    private QueueingBasicConsumer consumer;

    public RPCClient() {
        ConnectionFactory factory = new ConnectionFactory();
        factory.HostName = "localhost";
        connection = factory.CreateConnection();
        channel = connection.CreateModel();
        replyQueueName = channel.QueueDeclare();
        consumer = new QueueingBasicConsumer(channel);
        channel.BasicConsume(replyQueueName, false, consumer);
    }

    public string Call(string message) {
        string response = null;
        string corrId = Guid.NewGuid().ToString();
        IBasicProperties props = channel.CreateBasicProperties();
        props.ReplyTo = replyQueueName;
        props.CorrelationId = corrId;

        byte[] messageBytes = System.Text.Encoding.UTF8.GetBytes(message);
        channel.BasicPublish("", "rpc_queue", props, messageBytes);

        while (true) {
            BasicDeliverEventArgs ea =
                (BasicDeliverEventArgs)consumer.Queue.Dequeue();
            if (ea.BasicProperties.CorrelationId == corrId) {
                byte[] body = ea.Body;
                response = System.Text.Encoding.UTF8.GetString(body);
                channel.BasicCancel(consumer.ConsumerTag);
                break;
            }
        }
        return response;
    }
    public void Dispose() {
        connection.Close();
    }

    public static void Main() {
        Console.WriteLine(" [x] Requesting fib(30)");
        using (RPCClient rpcClient = new RPCClient()) {
            string response = rpcClient.Call("30");
            Console.WriteLine(" [.] Got '{0}'", response);
        }
    }
}
