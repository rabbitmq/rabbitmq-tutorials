Module Tutorial6RPCClient

    Class RPCClient
        Private connection As RabbitMQ.Client.IConnection
        Private channel As RabbitMQ.Client.IModel
        Private replyQueueName As String
        Private consumer As RabbitMQ.Client.QueueingBasicConsumer

        Sub New()
            Dim factory As New RabbitMQ.Client.ConnectionFactory
            factory.HostName = "localhost"
            connection = factory.CreateConnection
            channel = connection.CreateModel
            replyQueueName = channel.QueueDeclare
            consumer = New RabbitMQ.Client.QueueingBasicConsumer(channel)
            channel.BasicConsume(replyQueueName, False, consumer)
        End Sub

        Function Calll(message As String) As String
            Dim corrId = Guid.NewGuid.ToString
            Dim props = channel.CreateBasicProperties
            props.ReplyTo = replyQueueName
            props.CorrelationId = corrId
            Dim messageBytes = Text.Encoding.UTF8.GetBytes(message)
            channel.BasicPublish("", "rpc_queue", props, messageBytes)
            Do
                Dim ea As RabbitMQ.Client.Events.BasicDeliverEventArgs = consumer.Queue.Dequeue
                If ea.BasicProperties.CorrelationId = corrId Then
                    Return Text.Encoding.UTF8.GetString(ea.Body)
                End If
            Loop
        End Function

        Sub Close()
            connection.Close()
        End Sub
    End Class

    Sub Main()
        Dim rpcClient As New RPCClient
        Console.WriteLine(" [x] Requesting fib(30)")
        Dim response = rpcClient.Calll("30")
        Console.WriteLine(" [.] Got '{0}'", response)
        rpcClient.Close()
    End Sub

End Module
