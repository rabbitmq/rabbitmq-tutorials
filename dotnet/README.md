# Dotnet C# code for RabbitMQ tutorials

Here you can find the C# code examples for [RabbitMQ
tutorials](http://www.rabbitmq.com/getstarted.html).

To successfully use the examples you will need a running RabbitMQ server.

## Requirements

### Requirements on Windows

* [dotnet core](https://www.microsoft.com/net/core)

We're using the command line (start->run cmd.exe) to
compile and run -p the code. Alternatively you could [use Visual Studio](https://github.com/rabbitmq/rabbitmq-tutorials/tree/master/dotnet-visual-studio) and [NuGet package](https://www.nuget.org/packages/RabbitMQ.Client/), but this set of tutorials assumes
the command line.

### Requirements on Linux

* [dotnet core](https://www.microsoft.com/net/core)

## Code

Each command is best run -p in a separate console/terminal instance run from the root
of the tutorial directory.

#### [Tutorial one: "Hello World!"](http://www.rabbitmq.com/tutorial-one-dotnet.html)

    dotnet run -p Receive
    dotnet run -p Send

#### [Tutorial two: Work Queues](http://www.rabbitmq.com/tutorial-two-dotnet.html)

    dotnet run -p Worker
    dotnet run -p NewTask

#### [Tutorial three: Publish/Subscribe](http://www.rabbitmq.com/tutorial-three-dotnet.html)

    dotnet run -p ReceiveLogs
    dotnet run -p EmitLog

#### [Tutorial four: Routing](http://www.rabbitmq.com/tutorial-four-dotnet.html)

    dotnet run -p ReceiveLogsDirect info
    dotnet run -p EmitLogDirect

#### [Tutorial five: Topics](http://www.rabbitmq.com/tutorial-five-dotnet.html)

    dotnet run -p ReceiveLogsTopic anonymous.info
    dotnet run -p EmitLogTopic

#### [Tutorial six: RPC](http://www.rabbitmq.com/tutorial-six-dotnet.html)

    dotnet run -p RPCServer
    dotnet run -p RPCClient
