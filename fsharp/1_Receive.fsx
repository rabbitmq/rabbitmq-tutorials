#r "packages/RabbitMQ.Client/lib/net40/RabbitMQ.Client.dll"

open System
open RabbitMQ.Client
open RabbitMQ.Client.Events
open System.Text

let factory = new ConnectionFactory(HostName = "localhost")
(
    use connection = factory.CreateConnection()
    use channel = connection.CreateModel()

    channel.QueueDeclare("hello", false, false, false, null) |> ignore

    let consumer = new QueueingBasicConsumer(channel)
    channel.BasicConsume("hello", true, consumer) |> ignore

    printfn " [*] Waiting for messages. To exit press CTRL+C"

    let rec loop () : unit =
        let ea = consumer.Queue.Dequeue();

        let body = ea.Body
        let message = Encoding.UTF8.GetString(body)
        printfn " [x] Received %s" message

        loop ()

    loop ()
) // See more at: http://codiply.com/blog/rabbitmq-tutorials-in-f-sharp-one-hello-world#sthash.4zONPgAA.dpuf
