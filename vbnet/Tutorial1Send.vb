Module Tutorial1Send

    Sub Main()
        Dim factory As New RabbitMQ.Client.ConnectionFactory
        factory.HostName = "localhost"

        Using connection = factory.CreateConnection
            Using channel = connection.CreateModel
                channel.QueueDeclare("hello", False, False, False, Nothing)
                Dim message = "Hello World!"
                Dim body = Text.Encoding.UTF8.GetBytes(message)
                channel.BasicPublish("", "hello", Nothing, body)
                Console.WriteLine(" [x] Sent {0}", message)
            End Using
        End Using
    End Sub

End Module
