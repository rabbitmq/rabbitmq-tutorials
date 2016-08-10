Module Tutorial5ReceiveLogsTopic

    Sub Main(args As String())
        Dim factory As New RabbitMQ.Client.ConnectionFactory
        factory.HostName = "localhost"

        Using connection = factory.CreateConnection
            Using channel = connection.CreateModel
                channel.ExchangeDeclare("topic_logs", "topic")
                Dim queue_name = channel.QueueDeclare
                If args.Length < 1 Then
                    Console.Error.WriteLine("Usage: {0} [binding_key...]", Environment.GetCommandLineArgs()(0))
                    Environment.ExitCode = 1
                    Return
                End If
                For Each bindingKey In args
                    channel.QueueBind(queue_name, "topic_logs", bindingKey)
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
