# Dotnet C# code for RabbitMQ tutorials

Here you can find the C# code examples for [RabbitMQ
tutorials](https://www.rabbitmq.com/getstarted.html) using .NET 7.0.

You will also find a solution file for Visual Studio 2022.

To successfully use the examples you will need a running RabbitMQ server.

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

    dotnet run --project Receive/Receive.csproj
    dotnet run --project Send/Send.csproj

#### [Tutorial two: Work Queues](https://www.rabbitmq.com/tutorials/tutorial-two-dotnet.html)

    dotnet run --project Worker/Worker.csproj
    dotnet run --project NewTask/NewTask.csproj

#### [Tutorial three: Publish/Subscribe](https://www.rabbitmq.com/tutorials/tutorial-three-dotnet.html)

    dotnet run --project ReceiveLogs/ReceiveLogs.csproj
    dotnet run --project EmitLog/EmitLog.csproj

#### [Tutorial four: Routing](https://www.rabbitmq.com/tutorials/tutorial-four-dotnet.html)

    dotnet run --project ReceiveLogsDirect/ReceiveLogsDirect.csproj info
    dotnet run --project EmitLogDirect/EmitLogDirect.csproj

#### [Tutorial five: Topics](https://www.rabbitmq.com/tutorials/tutorial-five-dotnet.html)

    dotnet run --project ReceiveLogsTopic/ReceiveLogsTopic.csproj anonymous.info
    dotnet run --project EmitLogTopic/EmitLogTopic.csproj

#### [Tutorial six: RPC](https://www.rabbitmq.com/tutorials/tutorial-six-dotnet.html)

    dotnet run --project RPCServer/RPCServer.csproj
    dotnet run --project RPCClient/RPCClient.csproj

#### [Tutorial seven: Publisher Confirms](https://www.rabbitmq.com/tutorials/tutorial-seven-dotnet.html)

    dotnet run --project PublisherConfirms/PublisherConfirms.csproj
