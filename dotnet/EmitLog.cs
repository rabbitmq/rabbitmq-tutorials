using System;
using RabbitMQ.Client;
using System.Text;

class EmitLog
{
    public static void Main(string[] args)
    {
        var factory = new ConnectionFactory() { HostName = "localhost" };
        using (var connection = factory.CreateConnection())
        using (var channel = connection.CreateModel())
        {
            channel.ExchangeDeclare("logs", "fanout");

            var message = (args.Length > 0) ? string.Join(" ", args)
                                                           : "info: Hello World!";
            var body = Encoding.UTF8.GetBytes(message);
            channel.BasicPublish("logs", "", null, body);
            Console.WriteLine(" [x] Sent {0}", message);
        }
    }
}
