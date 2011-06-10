using System;
using RabbitMQ.Client;

class Send {
    public static void Main() {
        ConnectionFactory factory = new ConnectionFactory();
        factory.HostName = "localhost";
        using (IConnection connection = factory.CreateConnection())
        using (IModel channel = connection.CreateModel()) {
            channel.QueueDeclare("hello", false, false, false, null);

            string message = "Hello World!";
            byte[] body = System.Text.Encoding.UTF8.GetBytes(message);

            channel.BasicPublish("", "hello", null, body);
            Console.WriteLine(" [x] Sent {0}", message);
        }
    }
}
