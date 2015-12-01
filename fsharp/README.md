# Dotnet F# code for RabbitMQ tutorials

Here you can find the F# code examples for [RabbitMQ
tutorials](http://www.rabbitmq.com/getstarted.html).

To successfully use the examples you will need a running RabbitMQ server.

## Requirements

We're using the [Github Atom](atom.io) or [Visual Studio Code](code.visualstudio.com)
Download and install one of them on your computer.

Install **ionide* packages**:
Note that you can use Ctrl+Shift+P and `FSI: Send File` instead of working with console

Open the Command Palette in you editor with Ctrl+Shift+P or Cmd+Shift+P and execute following commands:

    Paket: Init
    Paket: Install

See more about F# [Paket Manager](http://fsprojects.github.io/Paket)

### Requirements on Windows

You need to ensure your system can find the F# Interactive `fsi.exe`,
you may need to add `c:\Program Files (x86)\Microsoft SDKs\F#\<version>\Framework\<version>\` (change .NET version
to fit your installation) to your `PATH`.

### Requirements on Linux

    sudo apt-get install mono-complete

### Requirements on Mac OS X

If you use [Homebrew](http://brew.sh):

    brew update
    brew install mono
    
Otherwise download mono from [here](http://www.mono-project.com/download/) and follow installation instructions.

## Code

#### [Tutorial one: "Hello World!"](http://www.rabbitmq.com/tutorial-one-dotnet.html)

##### Windows

    fsi 1_Send.fsx
    fsi 1_Receive.fsx

##### Linux/Mas OS

    fsharpi 1_Send.fsx
    fsharpi 1_Receive.fsx

#### [Tutorial two: Work Queues](http://www.rabbitmq.com/tutorial-two-dotnet.html)

##### Windows

    fsi 2_NewTask.fsx
    fsi 2_Worker.fsx

##### Linux/Mas OS

    fsharpi 2_NewTask.fsx
    fsharpi 2_Worker.fsx

#### [Tutorial three: Publish/Subscribe](http://www.rabbitmq.com/tutorial-three-dotnet.html)

##### Windows

    fsi 3_ReceiveLogs.fsx
    fsi 3_EmitLog.fsx

##### Linux/Mas OS

    fsharpi 3_ReceiveLogs.fsx
    fsharpi 3_EmitLog.fsx

#### [Tutorial four: Routing](http://www.rabbitmq.com/tutorial-four-dotnet.html)

##### Windows

    fsi 4_ReceiveLogsDirect.fsx
    fsi 4_EmitLogDirect.fsx

##### Linux/Mas OS

    fsharpi 4_ReceiveLogsDirect.fsx
    fsharpi 4_EmitLogDirect.fsx

#### [Tutorial five: Topics](http://www.rabbitmq.com/tutorial-five-dotnet.html)

##### Windows

    fsi 5_ReceiveLogsTopic.fsx
    fsi 5_EmitLogTopic.fsx

##### Linux/Mas OS

    fsharpi 5_ReceiveLogsTopic.fsx
    fsharpi 5_EmitLogTopic.fsx

#### [Tutorial six: RPC](http://www.rabbitmq.com/tutorial-six-dotnet.html)

##### Windows

    fsi 6_RPCServer.exe
    fsi 6_RPCClient.exe

##### Linux/Mas OS

    fsharpi 6_RPCServer.exe
    fsharpi 6_RPCClient.exe
