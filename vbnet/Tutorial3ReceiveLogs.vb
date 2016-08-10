Module Tutorial3ReceiveLogs

    Sub Main()
        Dim factory As New RabbitMQ.Client.ConnectionFactory
        factory.HostName = "localhost"

        Using connection = factory.CreateConnection
            Using channel = connection.CreateModel
                channel.ExchangeDeclare("logs", "fanout")
                Dim queue_name = channel.QueueDeclare
                channel.QueueBind(queue_name, "logs", "")
                Dim consumer As New RabbitMQ.Client.QueueingBasicConsumer(channel)
                channel.BasicConsume(queue_name, True, consumer)
                Console.WriteLine(" [*] Waiting for logs. To exit press CTRL+C")
                Do
                    Dim ea As RabbitMQ.Client.Events.BasicDeliverEventArgs = consumer.Queue.Dequeue
                    Dim body = ea.Body
                    Dim message = Text.Encoding.UTF8.GetString(body)
                    Console.WriteLine(" [x] {0}", message)
                Loop
            End Using
        End Using
    End Sub

End Module
