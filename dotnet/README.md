# Dotnet C# code for RabbitMQ tutorials

Here you can find the C# code examples for [RabbitMQ
tutorials](https://www.rabbitmq.com/getstarted.html) using .NET 7.0.

You will also find a solution file for Visual Studio 2022.

To successfully use the examples you will need a running RabbitMQ server.

You can easily set this up by [installing RabbitMQ](https://www.rabbitmq.com/docs/download).

## Requirements

### Requirements on Windows

* [dotnet core](https://www.microsoft.com/net/core)

We're using the command line (start->run cmd.exe) to
compile and run the code. Alternatively you could use Visual Studio, but this set of tutorials assumes
the command line.

### Requirements on Linux

* [dotnet core](https://www.microsoft.com/net/core)

### Code

Each command is best run in a separate console/terminal instance run from the root
of the tutorial directory.

#### [Tutorial one: "Hello World!"](https://www.rabbitmq.com/tutorials/tutorial-one-dotnet.html)

```bash
# terminal tab 1
dotnet run --project Receive/Receive.csproj

# terminal tab 2
dotnet run --project Send/Send.csproj
```

#### [Tutorial two: Work Queues](https://www.rabbitmq.com/tutorials/tutorial-two-dotnet.html)

```bash
# terminal tab 1
dotnet run --project Worker/Worker.csproj

# terminal tab 2
dotnet run --project Worker/Worker.csproj

# terminal tab 3
dotnet run --project NewTask/NewTask.csproj "First Message"
dotnet run --project NewTask/NewTask.csproj "Second Message"
dotnet run --project NewTask/NewTask.csproj "Third Message"
dotnet run --project NewTask/NewTask.csproj "Fourth Message"
dotnet run --project NewTask/NewTask.csproj "Fifth Message"
```

#### [Tutorial three: Publish/Subscribe](https://www.rabbitmq.com/tutorials/tutorial-three-dotnet.html)

```bash
# terminal tab 1
dotnet run --project ReceiveLogs/ReceiveLogs.csproj

# terminal tab 2
dotnet run --project ReceiveLogs/ReceiveLogs.csproj

# terminal tab 3
dotnet run --project EmitLog/EmitLog.csproj
```

#### [Tutorial four: Routing](https://www.rabbitmq.com/tutorials/tutorial-four-dotnet.html)

```bash
# terminal tab 1
dotnet run --project ReceiveLogsDirect/ReceiveLogsDirect.csproj warning error

# terminal tab 2
dotnet run --project ReceiveLogsDirect/ReceiveLogsDirect.csproj info warning error

# terminal tab 3
dotnet run --project EmitLogDirect/EmitLogDirect.csproj info "Run. Run. Or it will explode."
dotnet run --project EmitLogDirect/EmitLogDirect.csproj warning "Run. Run. Or it will explode."
dotnet run --project EmitLogDirect/EmitLogDirect.csproj error "Run. Run. Or it will explode."
```

#### [Tutorial five: Topics](https://www.rabbitmq.com/tutorials/tutorial-five-dotnet.html)

```bash
# terminal tab 1
# To receive all the logs:
dotnet run --project ReceiveLogsTopic/ReceiveLogsTopic.csproj "#"

# To receive all logs from the facility "kern":
dotnet run --project ReceiveLogsTopic/ReceiveLogsTopic.csproj "kern.*"

# Or if you want to hear only about "critical" logs:
dotnet run --project ReceiveLogsTopic/ReceiveLogsTopic.csproj "*.critical"

# You can create multiple bindings:
dotnet run --project ReceiveLogsTopic/ReceiveLogsTopic.csproj "kern.*" "*.critical"

# terminal tab 2
# And to emit a log with a routing key "kern.critical" type:
dotnet run --project EmitLogTopic/EmitLogTopic.csproj kern.critical "A critical kernel error"
```

#### [Tutorial six: RPC](https://www.rabbitmq.com/tutorials/tutorial-six-dotnet.html)

```bash
# terminal tab 1
# Our RPC service is now ready. We can start the server:
dotnet run --project RPCServer/RPCServer.csproj

# terminal tab 2
# To request a fibonacci number run the client:
dotnet run --project RPCClient/RPCClient.csproj
```

#### [Tutorial seven: Publisher Confirms](https://www.rabbitmq.com/tutorials/tutorial-seven-dotnet.html)

```bash
# terminal tab 1
dotnet run --project PublisherConfirms/PublisherConfirms.csproj
```
