using System;
using RabbitMQ.Client;

namespace Send {
    class Program {
        static void Main(string[] args) {
            ConnectionFactory factory = new ConnectionFactory();
            factory.HostName = "localhost";
            IConnection connection = factory.CreateConnection();
            IModel channel = connection.CreateModel();

            channel.QueueDeclare("hello");

            string message = "Hello World!";
            byte[] body = System.Text.Encoding.UTF8.GetBytes(message);

            channel.BasicPublish("", "hello", null, body);
            Console.WriteLine(" [x] Sent {0}", message);

            channel.Close();
            connection.Close();
        }
    }
}
