Module Tutorial4EmitLogDirect

    Sub Main(args As String())
        Dim factory As New RabbitMQ.Client.ConnectionFactory
        factory.HostName = "localhost"

        Using connection = factory.CreateConnection
            Using channel = connection.CreateModel
                channel.ExchangeDeclare("direct_logs", "direct")
                Dim severity = If(args.Length > 0, args(0), "info")
                Dim message = If(args.Length > 1, String.Join(" ", args.Skip(1).ToArray), "Hello World!")
                Dim body = Text.Encoding.UTF8.GetBytes(message)
                channel.BasicPublish("direct_logs", severity, Nothing, body)
                Console.WriteLine(" [x] Sent '{0}':'{1}'", severity, message)
            End Using
        End Using
    End Sub

End Module
