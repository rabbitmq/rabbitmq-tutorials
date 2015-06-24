using RabbitMQ.Client;
using RabbitMQ.Client.Events;
using System;
using System.Text;

class Receive
{
    public static void Main()
    {
        var factory = new ConnectionFactory() { HostName = "localhost" };
        using( var connection = factory.CreateConnection() )
        using( var channel = connection.CreateModel() )
        {
            channel.QueueDeclare( queue: "hello", durable: false, exclusive: false, autoDelete: false, arguments: null );

            var consumer = new QueueingBasicConsumer( channel );
            channel.BasicConsume( queue: "hello", noAck: true, consumer: consumer );

            Console.WriteLine( " [*] Waiting for messages. To exit press CTRL+C" );
            while( true )
            {
                var ea = (BasicDeliverEventArgs)consumer.Queue.Dequeue();

                var body = ea.Body;
                var message = Encoding.UTF8.GetString( body );
                Console.WriteLine( " [x] Received {0}", message );
            }
        }
    }
}
