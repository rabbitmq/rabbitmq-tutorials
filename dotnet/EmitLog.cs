using RabbitMQ.Client;

class Program {
    public static void Main(string[] args) {
        ConnectionFactory factory = new ConnectionFactory();
        factory.HostName = "localhost";
        using (IConnection connection = factory.CreateConnection())
        using (IModel channel = connection.CreateModel()) {
            channel.ExchangeDeclare("logs", "fanout");

            string message = (args.Length > 0) ? System.String.Join(" ", args)
                                               : "info: Hello World!";
            byte[] body = System.Text.Encoding.UTF8.GetBytes(message);
            channel.BasicPublish("logs", "", null, body);
            System.Console.WriteLine(" [x] Sent {0}", message);
        }
    }
}
