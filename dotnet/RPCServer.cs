using System;
using RabbitMQ.Client;
using RabbitMQ.Client.Events;
using System.Text;

class RPCServer
{
    public static void Main()
    {
        var factory = new ConnectionFactory() { HostName = "localhost" };
        using (var connection = factory.CreateConnection())
        {
            using (var channel = connection.CreateModel())
            {
                channel.QueueDeclare("rpc_queue", false, false, false, null);
                channel.BasicQos(0, 1, false);
                var consumer = new QueueingBasicConsumer(channel);
                channel.BasicConsume("rpc_queue", false, consumer);
                Console.WriteLine(" [x] Awaiting RPC requests");

                while (true)
                {
                    string response = null;
                    var ea =
                        (BasicDeliverEventArgs)consumer.Queue.Dequeue();

                    var body = ea.Body;
                    var props = ea.BasicProperties;
                    var replyProps = channel.CreateBasicProperties();
                    replyProps.CorrelationId = props.CorrelationId;

                    try
                    {
                        var message = Encoding.UTF8.GetString(body);
                        int n = int.Parse(message);
                        Console.WriteLine(" [.] fib({0})", message);
                        response = fib(n).ToString();
                    }
                    catch (Exception e)
                    {
                        Console.WriteLine(" [.] " + e.Message);
                        response = "";
                    }
                    finally
                    {
                        var responseBytes =
                            Encoding.UTF8.GetBytes(response);
                        channel.BasicPublish("", props.ReplyTo, replyProps,
                                             responseBytes);
                        channel.BasicAck(ea.DeliveryTag, false);
                    }
                }
            }
        }
    }

    private static int fib(int n)
    {
        if (n == 0 || n == 1) return n;
        return fib(n - 1) + fib(n - 2);
    }
}
