# Compile the code

    mvn compile

## Hello World

Execute the following command to send a hello world.

    mvn exec:java -Dexec.mainClass="Send"

Execute the following command to send a hello world.

    mvn exec:java -Dexec.mainClass="Send"

## Work Queues

Send a message which will be finshed immediately

    mvn exec:java -Dexec.mainClass="NewTask"

Send a message which need some second to execute each . is one second

    mvn exec:java -Dexec.mainClass="NewTask" -Dexec.args="rabbit1 ...."

Open worker. Add more workers to process, message will be handled in round robin way.

    mvn exec:java -Dexec.mainClass="Worker"

Kill a worker which is processing message. The message will be still in the queue and will be received by another worker.

## Publish and Subscriber

    mvn exec:java -Dexec.mainClass="EmitLog" -Dexec.args="rabbit1 msg1"

    mvn exec:java -Dexec.mainClass="ReceiveLogs"

## RPC

Open a shell and execute for the `RPCServer`. `rabbit1` is the host for RabbitMQ, without any execute parameters means to localhost.

    mvn exec:java -Dexec.mainClass="RPCServer"


Open another shell and execute for the `RPCClient`.

    mvn exec:java -Dexec.mainClass="RPCClient"
