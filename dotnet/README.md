# Dotnet C# code for RabbitMQ tutorials

Here you can find C# code examples for [RabbitMQ
tutorials](http://www.rabbitmq.com/getstarted.html).

## Requirements

### Requirements on Windows

You need the RabbitMQ dotnet client.

* Download [rabbitmq-dotnet-client-2.4.1-dotnet-3.0.zip](http://www.rabbitmq.com/releases/rabbitmq-dotnet-client/v2.4.1/rabbitmq-dotnet-client-2.4.1-dotnet-3.0.zip)
* Extract it and copy "RabbitMQ.Client.dll" to your working folder.

You also need to ensure your system can find the C# compiler `csc.exe`,
you may need to add `;C:\WINDOWS\Microsoft.NET\Framework\v3.5` to your
Path.

We suggest using the commandline (start->run cmd.exe) to
compile and run the code. Alternatively you could use Visual Studio, but
due to the nature of examples commandline is a preferred interface.

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

#### [Tutorial four: Routing](http://www.rabbitmq.com/tutorial-four-python.html)

##### Windows

    csc /r:"RabbitMQ.Client.dll" ReceiveLogsDirect.cs
    csc /r:"RabbitMQ.Client.dll" EmitLogDirect.cs

    ReceiveLogsDirect.exe
    EmitLogDirect.exe

##### Linux

    gmcs -r:lib/bin/RabbitMQ.Client.dll ReceiveLogsDirect.cs
    gmcs -r:lib/bin/RabbitMQ.Client.dll EmitLogDirect.cs

    MONO_PATH=lib/bin mono ReceiveLogsDirect.exe
    MONO_PATH=lib/bin mono EmitLogDirect.exe

#### [Tutorial five: Topics](http://www.rabbitmq.com/tutorial-five-python.html)

##### Windows

    csc /r:"RabbitMQ.Client.dll" ReceiveLogsTopic.cs
    csc /r:"RabbitMQ.Client.dll" EmitLogTopic.cs

    ReceiveLogsTopic.exe
    EmitLogTopic.exe

##### Linux

    gmcs -r:lib/bin/RabbitMQ.Client.dll ReceiveLogsTopic.cs
    gmcs -r:lib/bin/RabbitMQ.Client.dll EmitLogTopic.cs

    MONO_PATH=lib/bin mono ReceiveLogsTopic.exe
    MONO_PATH=lib/bin mono EmitLogTopic.exe

#### [Tutorial six: RPC](http://www.rabbitmq.com/tutorial-six-python.html)

##### Windows

    csc /r:"RabbitMQ.Client.dll" RPCServer.cs
    csc /r:"RabbitMQ.Client.dll" RPCClient.cs

    RPCServer.exe
    RPCClient.exe

##### Linux

    gmcs -r:lib/bin/RabbitMQ.Client.dll RPCServer.cs
    gmcs -r:lib/bin/RabbitMQ.Client.dll RPCClient.cs

    MONO_PATH=lib/bin mono RPCServer.exe
    MONO_PATH=lib/bin mono RPCClient.exe

