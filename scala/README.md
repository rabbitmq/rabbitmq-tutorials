# RabbitMQ Tutorials in Scala

This is a minimalistic Scala port of the RabbitMQ tutorials in Java.
The port is admittedly quite close to Java in terms of code style.
This is primarily to the fact that RabbitMQ Java client still supports
JDK 6 and doesn't have a lambda-friendly API.


## Compiling the Code

    mvn compile


## Running Examples

### Hello World

Execute the following command to receive a hello world:

    mvn exec:java -Dexec.mainClass="Recv"

Execute the following in a separate shell to send a hello world:

    mvn exec:java -Dexec.mainClass="Send"

### Work Queues

Send a message which will be finshed immediately:

    mvn exec:java -Dexec.mainClass="NewTask"

Send a message which need some second to execute each . is one second.

    mvn exec:java -Dexec.mainClass="NewTask" -Dexec.args="rabbit1 ...."

To start a worker (run in a separate shell):

    mvn exec:java -Dexec.mainClass="Worker"

Add more workers to the same queue, message will be distributed in the
round robin manner.

### Publish and Subscriber

    mvn exec:java -Dexec.mainClass="EmitLog" -Dexec.args="rabbit1 msg1"

    mvn exec:java -Dexec.mainClass="ReceiveLogs"

### RPC

In one shell:

    mvn exec:java -Dexec.mainClass="RPCServer"

In another shell:

    mvn exec:java -Dexec.mainClass="RPCClient"
