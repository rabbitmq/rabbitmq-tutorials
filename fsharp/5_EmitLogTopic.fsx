#r "packages/RabbitMQ.Client/lib/net40/RabbitMQ.Client.dll"

open System
open RabbitMQ.Client
open System.Text

let factory = new ConnectionFactory(HostName = "localhost")
(
    use connection = factory.CreateConnection()
    use channel = connection.CreateModel()

    channel.ExchangeDeclare("topic_logs", "topic")

    let (routingKey, message) = 
      match fsi.CommandLineArgs |> Array.toList with
       | [] -> "anonymous.info", "Hello, World!"
       | routingKey' :: messages -> 
            routingKey', String.concat " " messages

    let body = Encoding.UTF8.GetBytes message

    channel.BasicPublish(exchange = "topic_logs",
                         routingKey = routingKey,
                         basicProperties = null,
                         body = body) |> ignore

    printfn " [x] Sent '%s':'%s'" routingKey message
    printfn " Press [enter] to exit"
    Console.ReadLine() |> ignore
)