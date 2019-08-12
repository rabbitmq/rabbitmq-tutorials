using System;
using RabbitMQ.Client;
using System.Text;
using System.Threading;

class Program
{
    public static void Main(string[] args)
    {
        Thread.Sleep(2000);
        var factory = new ConnectionFactory() { HostName = "localhost" };
        using(var connection = factory.CreateConnection())
        using(var channel = connection.CreateModel())
        {
            channel.ExchangeDeclare(exchange: "logs", type: "fanout");

            var message = GetMessage(args);
            var body = Encoding.UTF8.GetBytes(message);
            channel.BasicPublish(exchange: "logs", routingKey: "", basicProperties: null, body: body);
            Console.WriteLine(" [x] Sent {0}", message);
        }

        Console.WriteLine(" Press [enter] to exit.");
        Console.ReadLine();
    }

    private static string GetMessage(string[] args)
    {
        return (( args.Length > 0) ? string.Join(" ", args) : "info: Hello World!");
    }
}
