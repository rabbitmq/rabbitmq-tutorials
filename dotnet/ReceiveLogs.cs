using RabbitMQ.Client;
using RabbitMQ.Client.Events;

class Program {
    static void Main() {
        ConnectionFactory factory = new ConnectionFactory();
        factory.HostName = "localhost";
        using (IConnection connection = factory.CreateConnection())
        using (IModel channel = connection.CreateModel()) {
            channel.ExchangeDeclare("logs", "fanout");

            string queue_name = channel.QueueDeclare();

            channel.QueueBind(queue_name, "logs", "");
            QueueingBasicConsumer consumer = new QueueingBasicConsumer(channel);
            channel.BasicConsume(queue_name, true, consumer);

            System.Console.WriteLine(" [*] Waiting for logs." +
                                     "To exit press CTRL+C");
            while(true) {
                BasicDeliverEventArgs ea =
                    (BasicDeliverEventArgs)consumer.Queue.Dequeue();

                byte[] body = ea.Body;
                string message = System.Text.Encoding.UTF8.GetString(body);
                System.Console.WriteLine(" [x] {0}", message);
            }
        }
    }
}
