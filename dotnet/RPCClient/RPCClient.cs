using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using RabbitMQ.Client;
using RabbitMQ.Client.Events;

class RPCClient
{
    private IConnection connection;
    private IModel channel;
    private string replyQueueName;
    private EventingBasicConsumer consumer;

    public RPCClient()
    {
        var factory = new ConnectionFactory() { HostName = "localhost" };
        connection = factory.CreateConnection();
        channel = connection.CreateModel();
        replyQueueName = channel.QueueDeclare().QueueName;
        consumer = new EventingBasicConsumer(channel);
        channel.BasicConsume(queue: replyQueueName, autoAck: true, consumer: consumer);
    }

        public string Call(string message)
        {
            var corrId = Guid.NewGuid().ToString();
            var props = channel.CreateBasicProperties();
            string response = null;

            props.ReplyTo = replyQueueName;
            props.CorrelationId = corrId;

            var messageBytes = Encoding.UTF8.GetBytes(message);
            channel.BasicPublish(exchange: "", routingKey: "rpc_queue", basicProperties: props, body: messageBytes);

            consumer.Received += (model, ea) =>
            {
                if (ea.BasicProperties.CorrelationId == corrId)
                {
                    response = Encoding.UTF8.GetString(ea.Body);
                }
            };

            return response;
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
