using RabbitMQ.Client;
using RabbitMQ.Client.Events;

namespace Receive {
    class Program {
        static void Main(string[] args) {
            ConnectionFactory factory = new ConnectionFactory();
            factory.HostName = "localhost";
            IConnection connection = factory.CreateConnection();
            IModel channel = connection.CreateModel();

            channel.QueueDeclare("hello");

            QueueingBasicConsumer consumer = new QueueingBasicConsumer(channel);
            channel.BasicConsume("hello", true, null, consumer);

            System.Console.WriteLine(" [*] Waiting for messages." +
                                     "To exit press CTRL+C");
            while(true) {
                BasicDeliverEventArgs ea =
                    (BasicDeliverEventArgs)consumer.Queue.Dequeue();

                byte[] body = ea.Body;
                string message = System.Text.Encoding.UTF8.GetString(body);
                System.Console.WriteLine(" [x] Received {0}", message);
            }
        }
    }
}
