using System;
using RabbitMQ.Client;
using RabbitMQ.Client.Events;

class RPCClient {
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
        channel.BasicConsume(replyQueueName, true, consumer);
    }

    public string Call(string message) {
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
                return System.Text.Encoding.UTF8.GetString(ea.Body);
            }
        }
    }

    public void Close() {
        connection.Close();
    }
}

class RPC {
    public static void Main() {
        RPCClient rpcClient = new RPCClient();

        Console.WriteLine(" [x] Requesting fib(30)");
        string response = rpcClient.Call("30");
        Console.WriteLine(" [.] Got '{0}'", response);

        rpcClient.Close();
    }
}
