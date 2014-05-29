using System;
using RabbitMQ.Client;
using RabbitMQ.Client.Events;
using System.Text;

class RPCClient
{
    private IConnection connection;
    private IModel channel;
    private string replyQueueName;
    private QueueingBasicConsumer consumer;

    public RPCClient()
    {
        var factory = new ConnectionFactory() { HostName = "localhost" };
        connection = factory.CreateConnection();
        channel = connection.CreateModel();
        replyQueueName = channel.QueueDeclare().QueueName;
        consumer = new QueueingBasicConsumer(channel);
        channel.BasicConsume(replyQueueName, true, consumer);
    }

    public string Call(string message)
    {
        var corrId = Guid.NewGuid().ToString();
        var props = channel.CreateBasicProperties();
        props.ReplyTo = replyQueueName;
        props.CorrelationId = corrId;

        var messageBytes = Encoding.UTF8.GetBytes(message);
        channel.BasicPublish("", "rpc_queue", props, messageBytes);

        while (true)
        {
            var ea = (BasicDeliverEventArgs)consumer.Queue.Dequeue();
            if (ea.BasicProperties.CorrelationId == corrId)
            {
                return Encoding.UTF8.GetString(ea.Body);
            }
        }
    }

    public void Close()
    {
        connection.Close();
    }
}

class RPC
{
    public static void Main()
    {
        var rpcClient = new RPCClient();

        Console.WriteLine(" [x] Requesting fib(30)");
        var response = rpcClient.Call("30");
        Console.WriteLine(" [.] Got '{0}'", response);

        rpcClient.Close();
    }
}
