using System;
using RabbitMQ.Client;
using RabbitMQ.Client.Events;
using System.Text;

class Program
{
    public static void Main( string[] args )
    {
        var rpcClient = new RPCClient();

        var n = args.Length > 0 ? args[0] : "30";
        Console.WriteLine( " [x] Requesting fib({0})", n );
        var response = rpcClient.Call( n );
        Console.WriteLine( " [.] Got '{0}'", response );

        rpcClient.Close();

        Console.WriteLine( " Press [enter] to exit." );
        Console.ReadLine();
    }
}
