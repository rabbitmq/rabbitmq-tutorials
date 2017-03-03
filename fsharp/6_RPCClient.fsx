#r "packages/RabbitMQ.Client/lib/net40/RabbitMQ.Client.dll"

open System
open System.Collections.Generic
open RabbitMQ.Client
open RabbitMQ.Client.Events
open System.Text
open System.Threading.Tasks

type RPCClient() =
    let factory = new ConnectionFactory(HostName = "localhost")
    let connection = factory.CreateConnection()
    let channel = connection.CreateModel()
    let replyQueueName = channel.QueueDeclare().QueueName
    let consumer = new QueueingBasicConsumer(channel)
    do channel.BasicConsume(queue = replyQueueName,
                         noAck = true,
                         consumer = consumer) |> ignore

    member __.Call (message:string) =
        let corrId = Guid.NewGuid() |> string
        let props = channel.CreateBasicProperties()
        props.ReplyTo <- replyQueueName
        props.CorrelationId <- corrId
        let messageBytes = Encoding.UTF8.GetBytes message
        channel.BasicPublish(exchange = "",
                             routingKey = "rpc_queue",
                             basicProperties = props,
                             body = messageBytes)
        let rec getBody (consumer:QueueingBasicConsumer) =
            let ea = consumer.Queue.Dequeue()
            if ea.BasicProperties.CorrelationId = corrId
            then Encoding.UTF8.GetString ea.Body
            else getBody consumer
        getBody consumer

    member __.Close() = connection.Close()
    
    interface IDisposable with
        member __.Dispose() = __.Close()

(
  use rpcClient = new RPCClient()
  printfn " [x] Requesting fib(30)"
  let response = rpcClient.Call "30"
  printfn " [.] Got '%s'" response
)
