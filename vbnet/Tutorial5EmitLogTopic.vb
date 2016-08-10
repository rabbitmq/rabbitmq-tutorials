Module Tutorial5EmitLogTopic

    Sub Main(args As String())
        Dim factory As New RabbitMQ.Client.ConnectionFactory
        factory.HostName = "localhost"

        Using connection = factory.CreateConnection
            Using channel = connection.CreateModel
                channel.ExchangeDeclare("topic_logs", "topic")
                Dim routingKey = If(args.Length > 0, args(0), "anonymous.info")
                Dim message = If(args.Length > 1, String.Join(" ", args.Skip(1).ToArray), "Hello World!")
                Dim body = Text.Encoding.UTF8.GetBytes(message)
                channel.BasicPublish("topic_logs", routingKey, Nothing, body)
                Console.WriteLine(" [x] Sent '{0}':'{1}'", routingKey, message)
            End Using
        End Using
    End Sub

End Module
