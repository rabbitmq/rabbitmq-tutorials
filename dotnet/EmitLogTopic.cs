using System.Linq;
using RabbitMQ.Client;

class EmitLogTopic {
    public static void Main(string[] args) {
        ConnectionFactory factory = new ConnectionFactory();
        factory.HostName = "localhost";
        using (IConnection connection = factory.CreateConnection())
        using (IModel channel = connection.CreateModel()) {
            channel.ExchangeDeclare("topic_logs", "topic");

            string routingKey = (args.Length > 0) ? args[0] : "anonymous.info";
            string message = (args.Length > 1) ? string.Join(" ", args.Skip(1).ToArray())
                                               : "Hello World!";
            byte[] body = System.Text.Encoding.UTF8.GetBytes(message);
            channel.BasicPublish("topic_logs", routingKey, null, body);
            System.Console.WriteLine(" [x] Sent '{0}':'{1}'", routingKey, message);
        }
    }
}
