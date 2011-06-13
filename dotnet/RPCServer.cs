using System;
using RabbitMQ.Client;
using RabbitMQ.Client.Events;

class RPCServer {
    public static void Main() {
        ConnectionFactory factory = new ConnectionFactory();
        factory.HostName = "localhost";
        using (IConnection connection = factory.CreateConnection())
        using (IModel channel = connection.CreateModel()) {
            channel.QueueDeclare("rpc_queue", false, false, false, null);
            channel.BasicQos(0, 1, false);
            QueueingBasicConsumer consumer = new QueueingBasicConsumer(channel);
            channel.BasicConsume("rpc_queue", false, consumer);
            Console.WriteLine(" [x] Awaiting RPC requests");

            while(true) {
                string response = null;
                BasicDeliverEventArgs ea =
                    (BasicDeliverEventArgs)consumer.Queue.Dequeue();

                byte[] body = ea.Body;
                IBasicProperties props = ea.BasicProperties;
                IBasicProperties replyProps = channel.CreateBasicProperties();
                replyProps.CorrelationId = props.CorrelationId;

                try {
                    string message = System.Text.Encoding.UTF8.GetString(body);
                    int n = int.Parse(message);
                    Console.WriteLine(" [.] fib({0})", message);
                    response = fib(n).ToString();
                } catch (Exception e) {
                    Console.WriteLine(" [.] " + e);
                    response = "";
                } finally {
                    byte[] responseBytes =
                        System.Text.Encoding.UTF8.GetBytes(response);
                    channel.BasicPublish("", props.ReplyTo, replyProps,
                                         responseBytes);
                    channel.BasicAck(ea.DeliveryTag, false);
                }
            }
        }
    }

    private static int fib(int n) {
        if (n == 0 || n == 1) return n;
        return fib(n - 1) + fib(n - 2);
    }
}
