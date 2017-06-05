using System;
using RabbitMQ.Client;
using RabbitMQ.Client;
using System.Text;
using RabbitMQ.Client.Framing;

class Send
{
    public static void Main()
    {
        var hostName = "localhost";
        ConnectionFactory factory = new ConnectionFactory();
        //
        // The next six lines are optional:
        factory.UserName = ConnectionFactory.DefaultUser;
        factory.Password = ConnectionFactory.DefaultPass;
        factory.VirtualHost = ConnectionFactory.DefaultVHost;
        factory.HostName = hostName;
        factory.Port     = AmqpTcpEndpoint.UseDefaultPort;
        //
        IConnection conn = factory.CreateConnection();
        //
        IModel ch = conn.CreateModel();
        //
        // ... use ch's IModel methods ...
        //
        ch.Close(Constants.ReplySuccess, "Closing the channel");
        conn.Close(Constants.ReplySuccess, "Closing the connection");;
    }
}
