#r "packages/RabbitMQ.Client/lib/net40/RabbitMQ.Client.dll"

open System
open RabbitMQ.Client
open RabbitMQ.Client.Events
open System.Text

let factory = new ConnectionFactory(HostName = "localhost")
(
    use connection = factory.CreateConnection()
    use channel = connection.CreateModel()

    channel.ExchangeDeclare("direct_logs", "direct")
    let queueName = channel.QueueDeclare().QueueName
    channel.QueueBind(queue = queueName,
                      exchange = "logs",
                      routingKey = " ")
    let args = fsi.CommandLineArgs
    if args.Length < 1 then
      printfn "Usage: %s [info] [warning] [error]" 
                fsi.CommandLineArgs.[0]
      printfn " Press [enter] to exit"
      Console.ReadLine() |> ignore
    
    for severity in args do
      channel.QueueBind(queue = queueName,
                        exchange = "direct_logs",
                        routingKey = severity)

    printfn " [*] Waiting for messages."

    let receive (ea:BasicDeliverEventArgs) =
      let message = Encoding.UTF8.GetString ea.Body
      let routingKey = ea.RoutingKey
      printfn " [x] Received '%s':'%s'" routingKey message

    let consumer =  EventingBasicConsumer channel
    consumer.Received.Add(receive)

    channel.BasicConsume(queue = queueName,
                         noAck = true,
                         consumer = consumer) |> ignore

    printfn " Press [enter] to exit"
    Console.ReadLine() |> ignore
)