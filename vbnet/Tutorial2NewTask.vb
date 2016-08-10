Module Tutorial2NewTask

    Sub Main(args As String())
        Dim factory As New RabbitMQ.Client.ConnectionFactory
        factory.HostName = "localhost"

        Using connection = factory.CreateConnection
            Using channel = connection.CreateModel
                channel.QueueDeclare("task_queue", True, False, False, Nothing)
                Dim message = If(args.Length > 0, String.Join(" ", args), "Hello World!")
                Dim body = Text.Encoding.UTF8.GetBytes(message)
                Dim properties = channel.CreateBasicProperties
                properties.DeliveryMode = 2
                channel.BasicPublish("", "task_queue", properties, body)
                Console.WriteLine(" [x] Sent {0}", message)
            End Using
        End Using
    End Sub

End Module
