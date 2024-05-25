# Dotnet Stream C# code for RabbitMQ tutorials

Here you can find the C# code examples for [RabbitMQ
tutorials](https://www.rabbitmq.com/getstarted.html) using .NET 8.0.

To successfully use the examples you will need a running RabbitMQ server with the [stream plugin enabled](https://www.rabbitmq.com/docs/stream#enabling-plugin).

You can easily set this up by [starting RabbitMQ with Streams Enabled](https://www.rabbitmq.com/blog/2021/07/19/rabbitmq-streams-first-application).

## Requirements

### Requirements on Windows

* [dotnet core](https://www.microsoft.com/net/core)

We're using the command line (start->run cmd.exe) to
compile and run the code. 

### Requirements on Linux

* [dotnet core](https://www.microsoft.com/net/core)

### Code

Each command is best run in a separate console/terminal instance run from the root
of the tutorial directory.

#### [Tutorial one: "Hello World!"](https://www.rabbitmq.com/tutorials/tutorial-one-dotnet-stream.html)

    dotnet run --project Receive/Receive.csproj
    dotnet run --project Send/Send.csproj
