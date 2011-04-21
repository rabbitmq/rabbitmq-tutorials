# Dotnet C# code for RabbitMQ tutorials

Here you can find C# code examples for [RabbitMQ
tutorials](http://www.rabbitmq.com/getstarted.html).

You'll need erlang installed, and also access to a [RabbitMQ server](http://www.rabbitmq.com/server.html). 
These are easy to [install](http://www.rabbitmq.com/install.html).


## Requirements

### Mono on Linux
You need Mono and RabbitMQ dotnet client.

    sudo apt-get install mono-devel
    mkdir lib
    cd lib
    wget http://www.rabbitmq.com/releases/rabbitmq-dotnet-client/v2.1.1/rabbitmq-dotnet-client-2.1.1-dotnet-3.0.zip
    unzip rabbitmq-dotnet-client-2.1.1-dotnet-3.0.zip
    cd ..

    
### Windows
You need the RabbitMQ dotnet client.

    Go to http://www.rabbitmq.com/releases/rabbitmq-dotnet-client/v2.1.1
    Download rabbitmq-dotnet-client-2.1.1-dotnet-3.0.zip
    Extract it to rabbitmq-dotnet-client-2.1.1-dotnet-3.0 in your working folder

    
## Code

For background, you can refer to [Tutorial one: "Hello World!"](http://www.rabbitmq.com/tutorial-one-python.html):


### Compile and run the C# examples using Mono on Linux.

    gmcs -r:lib/bin/RabbitMQ.Client.dll Send.cs
    MONO_PATH=lib/bin mono Send.exe

    gmcs -r:lib/bin/RabbitMQ.Client.dll Receive.cs
    MONO_PATH=lib/bin mono Receive.exe
    
    
### Compile the C# examples on Windows    

Ensure your system can find the c# compiler `csc.exe` 

    e.g. Add `;C:\WINDOWS\Microsoft.NET\Framework\v3.5` to your Path
    
If you put the whole client directory in your working directory:

    csc /r:".\rabbitmq-dotnet-client-2.1.1-dotnet-3.0\bin\RabbitMQ.Client.dll" Send.cs
    csc /r:".\rabbitmq-dotnet-client-2.1.1-dotnet-3.0\bin\RabbitMQ.Client.dll" Receive.cs

or, if you just copy the RabbitMQ.Client.dll client library to your working directory:

    csc /r:"RabbitMQ.Client.dll" Send.cs
    csc /r:"RabbitMQ.Client.dll" Receive.cs

or you could use MS Visual Studio.


### Run the example programs on Windows

Open 3 Command Prompt windows Start > Run... cmd 

Use `rabbitmqctl status` to check the server is running,
and `rabbitmqctl list_queues` to inspect the queue.

In the other two windows, navigate to your working directory to run the example client programs.

In another cmd window, send a message:
 
    Send.exe

Check queue identified as "hello" has 1 message.
In the final cmd window, set the listener going:

    Receive.exe

This will keep listening (Ctrl-C in this window will stop it) for messages. 
You should now see the first message, and the queue should be empty.
The Receive view should get any further messages you Send.
