Module Tutorial3EmitLog

    Sub Main(args As String())
        Dim factory As New RabbitMQ.Client.ConnectionFactory
        factory.HostName = "localhost"

        Using connection = factory.CreateConnection
            Using channel = connection.CreateModel
                channel.ExchangeDeclare("logs", "fanout")
                Dim message = If(args.Length > 0, String.Join(" ", args), "info: Hello World!")
                Dim body = Text.Encoding.UTF8.GetBytes(message)
                channel.BasicPublish("logs", "", Nothing, body)
                Console.WriteLine(" [x] Sent {0}", message)
            End Using
        End Using
    End Sub

End Module
