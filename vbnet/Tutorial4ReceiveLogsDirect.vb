Module Tutorial4ReceiveLogsDirect

    Sub Main(args As String())
        Dim factory As New RabbitMQ.Client.ConnectionFactory
        factory.HostName = "localhost"

        Using connection = factory.CreateConnection
            Using channel = connection.CreateModel
                channel.ExchangeDeclare("direct_logs", "direct")
                Dim queue_name = channel.QueueDeclare
                If args.Length < 1 Then
                    Console.Error.WriteLine("Usage: {0} [info] [warning] [error]", Environment.GetCommandLineArgs()(0))
                    Environment.ExitCode = 1
                    Return
                End If
                For Each severity In args
                    channel.QueueBind(queue_name, "direct_logs", severity)
                Next
                Console.WriteLine(" [*] Waiting for messages. To exit press CTRL+C")
                Dim consumer As New RabbitMQ.Client.QueueingBasicConsumer(channel)
                channel.BasicConsume(queue_name, True, consumer)
                Do
                    Dim ea As RabbitMQ.Client.Events.BasicDeliverEventArgs = consumer.Queue.Dequeue
                    Dim body = ea.Body
                    Dim message = Text.Encoding.UTF8.GetString(body)
                    Dim routingKey = ea.RoutingKey
                    Console.WriteLine(" [x] Received '{0}':'{1}'", routingKey, message)
                Loop
            End Using
        End Using
    End Sub

End Module
