using System;
using System.Linq;
using RabbitMQ.Client;

class EmitLogDirect {
    public static void Main(string[] args) {
        ConnectionFactory factory = new ConnectionFactory();
        factory.HostName = "localhost";
        using (IConnection connection = factory.CreateConnection())
        using (IModel channel = connection.CreateModel()) {
            channel.ExchangeDeclare("direct_logs", "direct");

            string severity = (args.Length > 0) ? args[0] : "info";
            string message = (args.Length > 1) ? string.Join(" ", args.Skip(1)
                                                                  .ToArray())
                                               : "Hello World!";
            byte[] body = System.Text.Encoding.UTF8.GetBytes(message);
            channel.BasicPublish("direct_logs", severity, null, body);
            Console.WriteLine(" [x] Sent '{0}':'{1}'", severity, message);
        }
    }
}
