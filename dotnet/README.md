# Dotnet C# code for RabbitMQ tutorials

Here you can find the C# code examples for [RabbitMQ
tutorials](http://www.rabbitmq.com/getstarted.html).

## Requirements

To run the examples you also need a running RabbitMQ server

### Requirements on Windows

You need the RabbitMQ dotnet client.

* Download [rabbitmq-dotnet-client-2.4.1-dotnet-3.0.zip](http://www.rabbitmq.com/releases/rabbitmq-dotnet-client/v2.4.1/rabbitmq-dotnet-client-2.4.1-dotnet-3.0.zip)
* Extract it and copy "RabbitMQ.Client.dll" to your working folder.

You also need to ensure your system can find the C# compiler `csc.exe`,
you may need to add `;C:\WINDOWS\Microsoft.NET\Framework\v3.5` to your
Path.

We're using the command line (start->run cmd.exe) to
compile and run the code. Alternatively you could use Visual Studio, but
due to the nature of examples we prefer the command line.

### Requirements on Linux

You need Mono and RabbitMQ dotnet client.

    sudo apt-get install mono-devel
    mkdir lib
    cd lib
    wget http://www.rabbitmq.com/releases/rabbitmq-dotnet-client/v2.4.1/rabbitmq-dotnet-client-2.4.1-dotnet-3.0.zip
    unzip rabbitmq-dotnet-client-2.4.1-dotnet-3.0.zip
    cd ..


## Code

#### [Tutorial one: "Hello World!"](http://www.rabbitmq.com/tutorial-one-python.html)

##### Windows

    csc /r:"RabbitMQ.Client.dll" Send.cs
    csc /r:"RabbitMQ.Client.dll" Receive.cs

    Send.exe
    Receive.exe

##### Linux

    gmcs -r:lib/bin/RabbitMQ.Client.dll Send.cs
    gmcs -r:lib/bin/RabbitMQ.Client.dll Receive.cs

    MONO_PATH=lib/bin mono Send.exe
    MONO_PATH=lib/bin mono Receive.exe


#### [Tutorial two: Work Queues](http://www.rabbitmq.com/tutorial-two-python.html)


##### Windows

    csc /r:"RabbitMQ.Client.dll" NewTask.cs
    csc /r:"RabbitMQ.Client.dll" Worker.cs

    NewTask.exe
    Worker.exe

##### Linux

    gmcs -r:lib/bin/RabbitMQ.Client.dll NewTask.cs
    gmcs -r:lib/bin/RabbitMQ.Client.dll Worker.cs

    MONO_PATH=lib/bin mono NewTask.exe
    MONO_PATH=lib/bin mono Worker.exe

#### [Tutorial three: Publish/Subscribe](http://www.rabbitmq.com/tutorial-three-python.html)

##### Windows

    csc /r:"RabbitMQ.Client.dll" ReceiveLogs.cs
    csc /r:"RabbitMQ.Client.dll" EmitLog.cs

    ReceiveLogs.exe
    EmitLog.exe

##### Linux

    gmcs -r:lib/bin/RabbitMQ.Client.dll ReceiveLogs.cs
    gmcs -r:lib/bin/RabbitMQ.Client.dll EmitLog.cs

    MONO_PATH=lib/bin mono ReceiveLogs.exe
    MONO_PATH=lib/bin mono EmitLog.exe
