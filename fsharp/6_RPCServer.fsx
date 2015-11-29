#r "packages/RabbitMQ.Client/lib/net40/RabbitMQ.Client.dll"

open System
open RabbitMQ.Client
open RabbitMQ.Client.Events
open System.Text

/// Assumes only valid positive integer input.
/// Don't expect this one to work for big numbers,
/// and it's probably the slowest recursive implementation possible.
let rec fib = function
| 0 -> 0
| 1 -> 1
| n -> fib (n - 1) + fib (n - 2)

let factory = new ConnectionFactory(HostName = "localhost")
(
    use connection = factory.CreateConnection()
    use channel = connection.CreateModel()

    channel.QueueDeclare(queue = "rpc_queue",
                         durable = false,
                         exclusive = false,
                         autoDelete = false,
                         arguments = null) |> ignore
    channel.BasicQos(0u, 1us, false)
    let consumer = QueueingBasicConsumer channel
    channel.BasicConsume(queue = "rpc_queue",
                         noAck = false,
                         consumer = consumer) |> ignore
    printfn " [x] Awaiting RPC requests"

    while true do
        let ea = consumer.Queue.Dequeue()

        let body = ea.Body
        let props = ea.BasicProperties
        let replyProps = channel.CreateBasicProperties()
        replyProps.CorrelationId <- props.CorrelationId

        let response =
            try
                let message = Encoding.UTF8.GetString body
                let n = int message
                printfn " [.] fib %s" message
                fib n |> string
            with
            | e ->
                e.ToString() |> printfn " [.] %s"
                ""

        let responseBytes = Encoding.UTF8.GetBytes response
        channel.BasicPublish(exchange = "",
                             routingKey = props.ReplyTo,
                             basicProperties = replyProps,
                             body = responseBytes)
        channel.BasicAck(deliveryTag = ea.DeliveryTag,
                         multiple = false)
)
