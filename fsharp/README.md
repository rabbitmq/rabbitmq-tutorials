# Dotnet F# code for RabbitMQ tutorials

Here you can find the F# code examples for [RabbitMQ
tutorials](http://www.rabbitmq.com/getstarted.html).

To successfully use the examples you will need a running RabbitMQ server.

## Requirements

We're using the [Github Atom](atom.io) or [Visual Studio Code](code.visualstudio.com)
Download and install one of them on your computer.

Additionally, if you install **ionide* packages**

- You'll don't need to donwload the .NET RabbitMQ.Client manually
- You can use Ctrl+Shift+P and FSI: Send File instead of working with console

Therefore open the Command Palette in you editor with Ctrl+Shift+P or Cmd+Shift+P and execute following commands:

    Paket: Init
    Paket: Install

See more about F# [Paket Manager](http://fsprojects.github.io/Paket)



### Requirements on Windows


You need the RabbitMQ dotnet client. Skip it if using [Paket](http://fsprojects.github.io/Paket)

* Download [RabbitMQ .NET client](https://github.com/rabbitmq/rabbitmq-server/releases/download/rabbitmq_v3_5_4/rabbitmq-dotnet-client-3.5.4-dotnet-4.0.zip)
* Extract it and copy "RabbitMQ.Client.dll" to your working folder.

You also need to ensure your system can find the F# Interactive `fsi.exe`,
you may need to add `c:\Program Files (x86)\Microsoft SDKs\F#\<version>\Framework\<version>\` (change .NET version
to fit your installation) to your `PATH`.

### Requirements on Linux

You need Mono and RabbitMQ dotnet client. Skip it if using [Paket](http://fsprojects.github.io/Paket)

    sudo apt-get install mono-devel
    mkdir lib
    cd lib
    wget https://github.com/rabbitmq/rabbitmq-server/releases/download/rabbitmq_v3_5_4/rabbitmq-dotnet-client-3.5.4-dotnet-4.0.zip
    unzip rabbitmq-dotnet-client-3.5.4-dotnet-4.0.zip
    cd ..

## Code

#### [Tutorial one: "Hello World!"](http://www.rabbitmq.com/tutorial-one-dotnet.html)

##### Windows

    fsi 1_Send.fsx
    fsi 1_Receive.fsx

##### Linux

    fsharpi 1_Send.fsx
    fsharpi 1_Receive.fsx

#### [Tutorial two: Work Queues](http://www.rabbitmq.com/tutorial-two-dotnet.html)


##### Windows

    fsi 2_NewTask.fsx
    fsi 2_Worker.fsx

##### Linux

    fsharpi 2_NewTask.fsx
    fsharpi 2_Worker.fsx

#### [Tutorial three: Publish/Subscribe](http://www.rabbitmq.com/tutorial-three-dotnet.html)

##### Windows

    fsi 3_ReceiveLogs.fsx
    fsi 3_EmitLog.fsx

##### Linux

    fsharpi 3_ReceiveLogs.fsx
    fsharpi 3_EmitLog.fsx

#### [Tutorial four: Routing](http://www.rabbitmq.com/tutorial-four-dotnet.html)

##### Windows

    fsi 4_ReceiveLogsDirect.fsx
    fsi 4_EmitLogDirect.fsx

##### Linux

    fsharpi 4_ReceiveLogsDirect.fsx
    fsharpi 4_EmitLogDirect.fsx

#### [Tutorial five: Topics](http://www.rabbitmq.com/tutorial-five-dotnet.html)

##### Windows

    fsi 5_ReceiveLogsTopic.fsx
    fsi 5_EmitLogTopic.fsx

##### Linux

    fsharpi 5_ReceiveLogsTopic.fsx
    fsharpi 5_EmitLogTopic.fsx

#### [Tutorial six: RPC](http://www.rabbitmq.com/tutorial-six-dotnet.html)

##### Windows

    fsi 6_RPCServer.exe
    fsi 6_RPCClient.exe

##### Linux

    fsharpi 6_RPCServer.exe
    fsharpi 6_RPCClient.exe
