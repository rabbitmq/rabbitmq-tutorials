using System;
using RabbitMQ.Client;
using System.Text;

class Program
{
    public static void Main( string[] args )
    {
        var factory = new ConnectionFactory() { HostName = "localhost" };
        using( var connection = factory.CreateConnection() )
        using( var channel = connection.CreateModel() )
        {
            channel.QueueDeclare( "task_queue", true, false, false, null );

            var message = GetMessage( args );
            var body = Encoding.UTF8.GetBytes( message );

            var properties = channel.CreateBasicProperties();
            properties.SetPersistent( true );

            channel.BasicPublish( "", "task_queue", properties, body );
            Console.WriteLine( " [x] Sent {0}", message );
        }

        Console.WriteLine( " Press [enter] to exit." );
        Console.ReadLine();
    }

    private static string GetMessage( string[] args )
    {
        return ( ( args.Length > 0 ) ? string.Join( " ", args ) : "Hello World!" );
    }
}
