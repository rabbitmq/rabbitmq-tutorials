# Dotnet C# code for RabbitMQ tutorials (AMQP 1.0)

Here you can find C# examples from the [RabbitMQ tutorials](https://www.rabbitmq.com/getstarted.html), using the [RabbitMQ AMQP 1.0 .NET client](https://github.com/rabbitmq/rabbitmq-amqp-dotnet-client) (`RabbitMQ.AMQP.Client` on NuGet) for RabbitMQ 4.x.

You need a RabbitMQ node running locally. The client requires **RabbitMQ 4.0 or newer** with AMQP 1.0 on port **5672**. The examples use the default `guest` user. See the [AMQP 1.0 client libraries overview](https://www.rabbitmq.com/client-libraries/amqp-client-libraries) and [AMQP in RabbitMQ](https://www.rabbitmq.com/docs/amqp).

There is a solution file for Visual Studio 2022 (`dotnet-amqp.sln`). From this directory, use `dotnet run --project` as below.

End-to-end smoke tests (broker on `localhost`, management plugin used to reset `hello` / `task_queue` when needed):

```bash
./test-tutorials.sh
```

## Requirements

- [.NET 8 SDK](https://dotnet.microsoft.com/download)
- NuGet package: `RabbitMQ.AMQP.Client` (see each `.csproj`; restore runs automatically with `dotnet run` or `dotnet build`)

## Code

Run each example from the `dotnet-amqp` directory.

#### Tutorial one: "Hello World!"

```bash
dotnet run --project Receive/Receive.csproj
dotnet run --project Send/Send.csproj
```

#### Tutorial two: Work Queues

```bash
dotnet run --project Worker/Worker.csproj
dotnet run --project Worker/Worker.csproj
dotnet run --project NewTask/NewTask.csproj "First Message"
dotnet run --project NewTask/NewTask.csproj "Second Message"
dotnet run --project NewTask/NewTask.csproj "Third Message"
dotnet run --project NewTask/NewTask.csproj "Fourth Message"
dotnet run --project NewTask/NewTask.csproj "Fifth Message"
```

#### Tutorial three: Publish/Subscribe

```bash
dotnet run --project ReceiveLogs/ReceiveLogs.csproj
dotnet run --project ReceiveLogs/ReceiveLogs.csproj
dotnet run --project EmitLog/EmitLog.csproj
```

#### Tutorial four: Routing

```bash
dotnet run --project ReceiveLogsDirect/ReceiveLogsDirect.csproj warning error
dotnet run --project ReceiveLogsDirect/ReceiveLogsDirect.csproj info warning error
dotnet run --project EmitLogDirect/EmitLogDirect.csproj info "Run. Run. Or it will explode."
dotnet run --project EmitLogDirect/EmitLogDirect.csproj warning "Run. Run. Or it will explode."
dotnet run --project EmitLogDirect/EmitLogDirect.csproj error "Run. Run. Or it will explode."
```

#### Tutorial five: Topics

```bash
dotnet run --project ReceiveLogsTopic/ReceiveLogsTopic.csproj "#"
dotnet run --project ReceiveLogsTopic/ReceiveLogsTopic.csproj "kern.*"
dotnet run --project ReceiveLogsTopic/ReceiveLogsTopic.csproj "*.critical"
dotnet run --project ReceiveLogsTopic/ReceiveLogsTopic.csproj "kern.*" "*.critical"
dotnet run --project EmitLogTopic/EmitLogTopic.csproj kern.critical "A critical kernel error"
```

#### Tutorial six: RPC

```bash
dotnet run --project RPCServer/RPCServer.csproj
dotnet run --project RPCClient/RPCClient.csproj
```

The client requests Fibonacci numbers for `0` through `31`, matching the Java AMQP 1.0 tutorial client.

#### Publisher confirms (AMQP 1.0 publish outcomes)

```bash
dotnet run --project PublisherConfirms/PublisherConfirms.csproj
```

#### AMQP 1.0 Direct Reply-To RPC

On RabbitMQ **4.2 and newer**, the .NET `Requester` uses [Direct Reply-To](https://www.rabbitmq.com/docs/direct-reply-to) when no explicit reply queue is set (see the client’s requester implementation). Older brokers fall back to a temporary reply queue.

```bash
dotnet run --project RpcAmqp10/RpcAmqp10.csproj
```

This sample runs a responder and a requester in one process; press CTRL+C to exit.

To learn more, see the [API documentation](https://rabbitmq.github.io/rabbitmq-amqp-dotnet-client/api/RabbitMQ.AMQP.Client.html).
