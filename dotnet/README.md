# Dotnet C# code for RabbitMQ tutorials

Here you can find a C# code examples from [RabbitMQ
tutorials](http://www.rabbitmq.com/getstarted.html).


Here we present steps to run the examples using Mono on Linux.

## Requirements

You need Mono and RabbitMQ dotnet client.

    sudo apt-get install mono-devel
    mkdir lib
    cd lib
    wget http://www.rabbitmq.com/releases/rabbitmq-dotnet-client/v2.1.1/rabbitmq-dotnet-client-2.1.1-dotnet-3.0.zip
    unzip rabbitmq-dotnet-client-2.1.1-dotnet-3.0.zip
    cd ..


## Code

[Tutorial one: "Hello World!"](http://www.rabbitmq.com/tutorial-one-python.html):

    gmcs -r:lib/bin/RabbitMQ.Client.dll Send.cs
    MONO_PATH=lib/bin mono Send.exe

    gmcs -r:lib/bin/RabbitMQ.Client.dll Receive.cs
    MONO_PATH=lib/bin mono Receive.exe
