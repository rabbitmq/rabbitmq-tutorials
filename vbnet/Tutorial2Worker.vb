Module Tutorial2Worker

    Sub Main()
        Dim factory As New RabbitMQ.Client.ConnectionFactory
        factory.HostName = "localhost"

        Using connection = factory.CreateConnection
            Using channel = connection.CreateModel
                channel.QueueDeclare("task_queue", True, False, False, Nothing)
                channel.BasicQos(0, 1, False)
                Dim consumer As New RabbitMQ.Client.QueueingBasicConsumer(channel)
                channel.BasicConsume("task_queue", False, consumer)
                Console.WriteLine(" [*] Waiting for messages. To exit press CTRL+C")
                Do
                    Dim ea As RabbitMQ.Client.Events.BasicDeliverEventArgs = consumer.Queue.Dequeue
                    Dim body = ea.Body
                    Dim message = Text.Encoding.UTF8.GetString(body)
                    Console.WriteLine(" [x] Received {0}", message)
                    Dim dots = message.Split(".").Length - 1
                    Threading.Thread.Sleep(dots * 1000)
                    Console.WriteLine(" [x] Done")
                    channel.BasicAck(ea.DeliveryTag, False)
                Loop
            End Using
        End Using
    End Sub

End Module
