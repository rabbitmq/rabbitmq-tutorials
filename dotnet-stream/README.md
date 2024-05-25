# Dotnet Stream C# code for RabbitMQ tutorials

Here you can find the C# code examples for [RabbitMQ
tutorials](https://www.rabbitmq.com/getstarted.html) using .NET 8.0.

To successfully use the examples you will need a running RabbitMQ server with the [stream plugin enabled](https://www.rabbitmq.com/docs/stream#enabling-plugin).

See [First Application With RabbitMQ Streams](https://www.rabbitmq.com/blog/2021/07/19/rabbitmq-streams-first-application), [Stream plugin documentation](https://www.rabbitmq.com/docs/stream) and [how to preconfigure plugins](https://www.rabbitmq.com/docs/plugins#enabled-plugins-file).

## Requirements

### Requirements on Windows

* [dotnet core](https://www.microsoft.com/net/core)

We're using the command line (`Start` -> `Run cmd.exe`) to
compile and run the code.

### Requirements on Linux

* [dotnet core](https://www.microsoft.com/net/core)

### Code

Each command is best run in a separate console/terminal instance run from the root
of the tutorial directory.

#### [Tutorial one: "Hello World!"](https://www.rabbitmq.com/tutorials/tutorial-one-dotnet-stream.html)

    dotnet run --project Receive/Receive.csproj
    dotnet run --project Send/Send.csproj
