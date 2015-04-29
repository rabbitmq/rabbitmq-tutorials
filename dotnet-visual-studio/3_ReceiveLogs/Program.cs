using System;
using RabbitMQ.Client;
using RabbitMQ.Client.Events;
using System.Text;

class Program
{
    public static void Main()
    {
        var factory = new ConnectionFactory() { HostName = "localhost" };
        using( var connection = factory.CreateConnection() )
        using( var channel = connection.CreateModel() )
        {
            channel.ExchangeDeclare( "logs", "fanout" );

            var queueName = channel.QueueDeclare().QueueName;

            channel.QueueBind( queueName, "logs", "" );
            var consumer = new QueueingBasicConsumer( channel );
            channel.BasicConsume( queueName, true, consumer );

            Console.WriteLine( " [*] Waiting for logs. To exit press CTRL+C" );
            while( true )
            {
                var ea = (BasicDeliverEventArgs)consumer.Queue.Dequeue();

                var body = ea.Body;
                var message = Encoding.UTF8.GetString( body );
                Console.WriteLine( " [x] {0}", message );
            }
        }
    }
}
