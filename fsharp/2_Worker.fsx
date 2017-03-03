#r "packages/RabbitMQ.Client/lib/net40/RabbitMQ.Client.dll"

open System
open RabbitMQ.Client
open RabbitMQ.Client.Events
open System.Text
open System.Threading


let factory = new ConnectionFactory(HostName = "localhost")
(
    use connection = factory.CreateConnection()
    use channel = connection.CreateModel()

    channel.BasicQos(0u, 1us, false)

    printfn " [*] Waiting for messages."

    let receive (ea:BasicDeliverEventArgs) =
        let message = Encoding.UTF8.GetString ea.Body
        printfn "[x] Received %s" message

        let dots = message.Split([|'.'|]).Length - 1
        Thread.Sleep (dots * 1000)

        printfn " [x] Done"

        channel.BasicAck(deliveryTag = ea.DeliveryTag, multiple = false)

    let consumer =  EventingBasicConsumer channel
    consumer.Received.Add(receive)
    
    channel.BasicConsume(
      queue = "task_queue",
      noAck = false,
      consumer = consumer) |> ignore

    printfn " Press [enter] to exit"
    Console.ReadLine() |> ignore
)
