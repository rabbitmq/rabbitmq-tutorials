#r "packages/RabbitMQ.Client/lib/net40/RabbitMQ.Client.dll"

open System
open RabbitMQ.Client
open System.Text

let factory = new ConnectionFactory(HostName = "localhost")
(
    use connection = factory.CreateConnection()
    use channel = connection.CreateModel()

    channel.ExchangeDeclare("direct_logs", "direct")


    let (severity, message) = 
      match fsi.CommandLineArgs |> Array.toList with
       | [] -> "info", "Hello, World!"
       | severity' :: messages -> 
            severity', String.concat " " messages

    let body = Encoding.UTF8.GetBytes message

    channel.BasicPublish(exchange = "direct_logs",
                         routingKey = severity,
                         basicProperties = null,
                         body = body) |> ignore

    printfn " [x] Sent '%s':'%s'" severity message
    printfn " Press [enter] to exit"
    Console.ReadLine() |> ignore
)