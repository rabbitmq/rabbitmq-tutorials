# VB.Net code for RabbitMQ tutorials

Here you can find a VB.Net code examples from [RabbitMQ
tutorials](http://www.rabbitmq.com/getstarted.html).

To successfully use the examples you will need a running RabbitMQ server.

## Requirements

You need the RabbitMQ.Client NuGet package and Visual Studio 2010 / 2012.

To compile the source code:
* Create a new console project in Visual Studio.
* Add NuGet package RabbitMQ.Client.
* Add one of the provided modules, for example Tutorial1Send.vb.
* Remove the default Module1.vb.
* Make sure the startup object is the added module (for example the Tutorial1Send.vb added above).
* Run.

## Code

[Tutorial one: "Hello World!"](http://www.rabbitmq.com/tutorial-one-python.html):

    Tutorial1Send.vb
    Tutorial1Receive.vb

[Tutorial two: Work Queues](http://www.rabbitmq.com/tutorial-two-python.html):

    Tutorial2NewTask.vb
    Tutorial2Worker.vb

[Tutorial three: Publish/Subscribe](http://www.rabbitmq.com/tutorial-three-python.html)

    Tutorial3ReceiveLogs.vb
    Tutorial3EmitLog.vb

[Tutorial four: Routing](http://www.rabbitmq.com/tutorial-four-python.html)

    Tutorial4ReceiveLogsDirect.vb
    Tutorial4EmitLogDirect.vb

[Tutorial five: Topics](http://www.rabbitmq.com/tutorial-five-python.html)

    Tutorial5ReceiveLogsTopic.vb
    Tutorial5EmitLogTopic.vb

[Tutorial six: RPC](http://www.rabbitmq.com/tutorial-six-python.html)

    Tutorial6RPCServer.vb
    Tutorial6RPCClient.vb
