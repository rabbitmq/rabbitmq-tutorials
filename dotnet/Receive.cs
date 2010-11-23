using System;
using RabbitMQ.Client;
using RabbitMQ.Client.Events;

namespace Receive {
    class Program {
        static void Main(string[] args) {
            ConnectionFactory factory = new ConnectionFactory();
            factory.HostName = "localhost";
            IConnection connection = factory.CreateConnection();
            IModel channel = connection.CreateModel();

            channel.QueueDeclare("test");

            QueueingBasicConsumer consumer = new QueueingBasicConsumer(channel);
            channel.BasicConsume("test", true, null, consumer);

            Console.WriteLine(" [*] Waiting for messages. To exit press CTRL+C");
            while(true) {
                BasicDeliverEventArgs ea =
                    (BasicDeliverEventArgs)consumer.Queue.Dequeue();

                byte[] body = ea.Body;
                string message = System.Text.Encoding.UTF8.GetString(body);
                Console.WriteLine(" [x] Received {0}", message);
            }
        }
    }
}
