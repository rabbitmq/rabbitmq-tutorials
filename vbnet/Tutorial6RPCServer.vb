Module Tutorial6RPCServer

    Sub Main()
        Dim factory As New RabbitMQ.Client.ConnectionFactory
        factory.HostName = "localhost"

        Using connection = factory.CreateConnection
            Using channel = connection.CreateModel
                channel.QueueDeclare("rpc_queue", False, False, False, Nothing)
                channel.BasicQos(0, 1, False)
                Dim consumer As New RabbitMQ.Client.QueueingBasicConsumer(channel)
                channel.BasicConsume("rpc_queue", False, consumer)
                Console.WriteLine(" [x] Awaiting RPC requests")
                Do
                    Dim response = ""
                    Dim ea As RabbitMQ.Client.Events.BasicDeliverEventArgs = consumer.Queue.Dequeue
                    Dim body = ea.Body
                    Dim props = ea.BasicProperties
                    Dim replyProps = channel.CreateBasicProperties
                    replyProps.CorrelationId = props.CorrelationId
                    Try
                        Dim message = Text.Encoding.UTF8.GetString(body)
                        Dim n As Integer = Integer.Parse(message)
                        Console.WriteLine(" [.] fib({0})", message)
                        response = fib(n).ToString
                    Catch ex As Exception
                        Console.WriteLine(" [.] " + ex.ToString)
                        response = ""
                    Finally
                        Dim responseBytes = Text.Encoding.UTF8.GetBytes(response)
                        channel.BasicPublish("", props.ReplyTo, replyProps, responseBytes)
                        channel.BasicAck(ea.DeliveryTag, False)
                    End Try
                Loop
            End Using
        End Using
    End Sub

    Private Function fib(n As Integer) As Integer
        If n = 0 Or n = 1 Then
            Return n
        End If
        Return fib(n - 1) + fib(n - 2)
    End Function

End Module
