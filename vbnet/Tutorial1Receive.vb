Module Tutorial1Receive

    Sub Main()
        Dim factory As New RabbitMQ.Client.ConnectionFactory
        factory.HostName = "localhost"

        Using connection = factory.CreateConnection
            Using channel = connection.CreateModel
                channel.QueueDeclare("hello", False, False, False, Nothing)
                Dim consumer As New RabbitMQ.Client.QueueingBasicConsumer(channel)
                channel.BasicConsume("hello", True, consumer)
                Console.WriteLine(" [*] Waiting for messages. To exit press CTRL+C")
                Do
                    Dim ea As RabbitMQ.Client.Events.BasicDeliverEventArgs = consumer.Queue.Dequeue
                    Dim body = ea.Body
                    Dim message = Text.Encoding.UTF8.GetString(body)
                    Console.WriteLine(" [x] Received {0}", message)
                Loop
            End Using
        End Using
    End Sub

End Module
