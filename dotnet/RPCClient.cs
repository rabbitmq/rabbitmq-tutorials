using System;
using RabbitMQ.Client;
using RabbitMQ.Client.Events;

class RPCClient {
    private static string RpcCall(string message) {
        string response = null;
        ConnectionFactory factory = new ConnectionFactory();
        factory.HostName = "localhost";
        using (IConnection connection = factory.CreateConnection())
        using (IModel channel = connection.CreateModel()) {
            string replyQueueName = channel.QueueDeclare();
            QueueingBasicConsumer consumer = new QueueingBasicConsumer(channel);
            channel.BasicConsume(replyQueueName, false, consumer);

            string corrId = Guid.NewGuid().ToString();
            IBasicProperties props = channel.CreateBasicProperties();
            props.ReplyTo = replyQueueName;
            props.CorrelationId = corrId;

            byte[] messageBytes = System.Text.Encoding.UTF8.GetBytes(message);
            channel.BasicPublish("", "rpc_queue", props, messageBytes);

            while (true) {
                BasicDeliverEventArgs ea = (BasicDeliverEventArgs)consumer.Queue.Dequeue();
                if (ea.BasicProperties.CorrelationId == corrId) {
                    byte[] body = ea.Body;
                    response = System.Text.Encoding.UTF8.GetString(body);
                    channel.BasicCancel(consumer.ConsumerTag);
                    break;
                }
            }
            return response;
        }
    }

    public static void Main() {
        Console.WriteLine(" [x] Requesting fib(30)");
        string response = RpcCall("30");
        Console.WriteLine(" [.] Got '{0}'", response);
    }
}