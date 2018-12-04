# RabbitMQ Tutorials in Scala

This is a minimalistic Scala port of the RabbitMQ tutorials in Java.
The port is admittedly quite close to Java in terms of code style.
This is primarily to the fact that RabbitMQ Java client still supports
JDK 6 and doesn't have a lambda-friendly API.


## Compiling the Code

    ./mvnw compile


## Running Examples

### Hello World

Execute the following command to receive a hello world:

    ./mvnw exec:java -Dexec.mainClass="Recv"

Execute the following in a separate shell to send a hello world:

    ./mvnw exec:java -Dexec.mainClass="Send"

### Work Queues

Send a message which will be finished immediately:

    ./mvnw exec:java -Dexec.mainClass="NewTask"

Send a message which need some second to execute each . is one second.

    ./mvnw exec:java -Dexec.mainClass="NewTask" -Dexec.args="rabbit1 ...."

To start a worker (run in a separate shell):

    ./mvnw exec:java -Dexec.mainClass="Worker"

Add more workers to the same queue, message will be distributed in the
round robin manner.

### Publish and Subscriber

    ./mvnw exec:java -Dexec.mainClass="ReceiveLogs"

    ./mvnw exec:java -Dexec.mainClass="EmitLog" -Dexec.args="rabbit1 msg1"


### Routing

    ./mvnw exec:java -Dexec.mainClass="ReceiveLogsDirect" -Dexec.args="info warning error"

    ./mvnw exec:java -Dexec.mainClass="EmitLogDirect" -Dexec.args="error Run. Run. Or it will explode."

### Topics

    ./mvnw exec:java -Dexec.mainClass="ReceiveLogsTopic" -Dexec.args="#"

    ./mvnw exec:java -Dexec.mainClass="ReceiveLogsTopic" -Dexec.args="kern.*"

    ./mvnw exec:java -Dexec.mainClass="ReceiveLogsTopic" -Dexec.args="*.critical"

    ./mvnw exec:java -Dexec.mainClass="ReceiveLogsTopic" -Dexec.args="kern.* *.critical"

    ./mvnw exec:java -Dexec.mainClass="EmitLogTopic" -Dexec.args="kern.critical A critical kernel error"

### RPC

In one shell:

    ./mvnw exec:java -Dexec.mainClass="RPCServer"

In another shell:

    ./mvnw exec:java -Dexec.mainClass="RPCClient"
