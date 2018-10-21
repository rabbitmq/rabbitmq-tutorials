using System;
using System.Text;
using System.Threading.Tasks;
using RabbitMQ.Client;
using RabbitMQ.Client.Events;

internal class Program
{
    public static void Main(string[] args)
    {
        Console.WriteLine("RPC Client");
        string n = args.Length > 0 ? args[0] : "30";
        Task t = InvokeAsync(n);
        t.Wait();

        Console.WriteLine(" Press [enter] to exit.");
        Console.ReadLine();
    }

    private static async Task InvokeAsync(string n)
    {
        var rnd = new Random(Guid.NewGuid().GetHashCode());
        var rpcClient = new RpcClient();

        Console.WriteLine(" [x] Requesting fib({0})", n);
        var response = await rpcClient.CallAsync(n.ToString());
        Console.WriteLine(" [.] Got '{0}'", response);

        rpcClient.Close();
    }
}
