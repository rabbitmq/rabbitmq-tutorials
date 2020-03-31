using System;
using RabbitMQ.Client;
using RabbitMQ.Client.Events;
using System.Text;
using System.Threading;

class Program
{
    public static void Main()
    {
        var factory = new ConnectionFactory() { HostName = "localhost" };
        using(var connection = factory.CreateConnection())
        using(var channel = connection.CreateModel())
        {
            channel.QueueDeclare(queue: "task_queue", durable: true, exclusive: false, autoDelete: false, arguments: null);

            channel.BasicQos(prefetchSize: 0, prefetchCount: 1, global: false);

            Console.WriteLine(" [*] Waiting for messages.");

            var consumer = new EventingBasicConsumer(channel);
            consumer.Received += (model, ea) =>
            {
                byte[] body = ea.Body.ToArray();
                var message = Encoding.UTF8.GetString(body);
                Console.WriteLine(" [x] Received {0}", message);

                int dots = message.Split('.').Length - 1;
                Thread.Sleep(dots * 1000);

                Console.WriteLine(" [x] Done");

                channel.BasicAck(deliveryTag: ea.DeliveryTag, multiple: false);
            };
            channel.BasicConsume(queue: "task_queue", autoAck: false, consumer: consumer);

            Console.WriteLine(" Press [enter] to exit.");
            Console.ReadLine();
        }
    }
}
