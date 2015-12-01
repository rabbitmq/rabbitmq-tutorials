#r "packages/RabbitMQ.Client/lib/net40/RabbitMQ.Client.dll"

open System
open RabbitMQ.Client
open System.Text

let getMessage = function
  | [||] -> "Hello World"
  | argv -> String.concat " " argv

let factory = new ConnectionFactory(HostName = "localhost")
(
    use connection = factory.CreateConnection()
    use channel = connection.CreateModel()
    channel.QueueDeclare(queue = "task_queue",
                         durable = true,
                         exclusive = false,
                         autoDelete = false,
                         arguments = null) |> ignore
    let message = getMessage fsi.CommandLineArgs
    let body = Encoding.UTF8.GetBytes message

    let properties = channel.CreateBasicProperties(Persistent = true)
    channel.BasicPublish(exchange = "",
                         routingKey = "task_queue",
                         basicProperties = properties,
                         body = body) |> ignore
    printfn " [x] Sent %s" message
    printfn " Press [enter] to exit"
    Console.ReadLine() |> ignore
)
