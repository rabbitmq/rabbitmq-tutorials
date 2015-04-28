using System;
using System.Linq;
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
            channel.ExchangeDeclare( "direct_logs", "direct" );

            var severity = ( args.Length > 0 ) ? args[0] : "info";
            var message = ( args.Length > 1 ) ? string.Join( " ", args.Skip( 1 ).ToArray() ) : "Hello World!";
            var body = Encoding.UTF8.GetBytes( message );
            channel.BasicPublish( "direct_logs", severity, null, body );
            Console.WriteLine( " [x] Sent '{0}':'{1}'", severity, message );
        }

        Console.WriteLine( " Press [enter] to exit." );
        Console.ReadLine();
    }
}
