# Java code for RabbitMQ tutorials (AMQP 1.0 client)

Here you can find Java examples for the [RabbitMQ tutorials](https://www.rabbitmq.com/getstarted.html) using the [RabbitMQ AMQP 1.0 Java client](https://rabbitmq.github.io/rabbitmq-amqp-java-client/stable/htmlsingle/).

You need a RabbitMQ node running locally. The client requires **RabbitMQ 4.0 or newer** with AMQP 1.0 (enabled by default on port **5672**). The examples use the default `guest` user. See also the [AMQP 1.0 client libraries overview](https://www.rabbitmq.com/client-libraries/amqp-client-libraries).

This directory is a self-contained [Maven](https://maven.apache.org/) project. Use `./mvnw` on Unix-like systems or `mvnw.cmd` on Windows.

Run integration tests with `./mvnw test` (they expect a broker on `localhost`).

## Code

#### Tutorial one: "Hello World!"

```shell
# terminal tab 1
./mvnw compile exec:java -Dexec.mainClass=Recv

# terminal tab 2
./mvnw compile exec:java -Dexec.mainClass=Send
```

#### Tutorial two: Work Queues

```shell
# terminal tab 1
./mvnw compile exec:java -Dexec.mainClass=Worker

# terminal tab 2
./mvnw compile exec:java -Dexec.mainClass=Worker

# terminal tab 3
./mvnw compile exec:java -Dexec.mainClass=NewTask -Dexec.args="First Message"
./mvnw compile exec:java -Dexec.mainClass=NewTask -Dexec.args="Second Message"
./mvnw compile exec:java -Dexec.mainClass=NewTask -Dexec.args="Third Message"
./mvnw compile exec:java -Dexec.mainClass=NewTask -Dexec.args="Fourth Message"
./mvnw compile exec:java -Dexec.mainClass=NewTask -Dexec.args="Fifth Message"
```

#### Tutorial three: Publish/Subscribe

```shell
# terminal tab 1
./mvnw compile exec:java -Dexec.mainClass=ReceiveLogs

# terminal tab 2
./mvnw compile exec:java -Dexec.mainClass=ReceiveLogs

# terminal tab 3
./mvnw compile exec:java -Dexec.mainClass=EmitLog
```

#### Tutorial four: Routing

```shell
# terminal tab 1
./mvnw compile exec:java -Dexec.mainClass=ReceiveLogsDirect -Dexec.args="warning error"

# terminal tab 2
./mvnw compile exec:java -Dexec.mainClass=ReceiveLogsDirect -Dexec.args="info warning error"

# terminal tab 3
./mvnw compile exec:java -Dexec.mainClass=EmitLogDirect -Dexec.args="info Run. Run. Or it will explode."
./mvnw compile exec:java -Dexec.mainClass=EmitLogDirect -Dexec.args="warning Run. Run. Or it will explode."
./mvnw compile exec:java -Dexec.mainClass=EmitLogDirect -Dexec.args="error Run. Run. Or it will explode."
```

#### Tutorial five: Topics

```shell
# terminal tab 1
# To receive all the logs:
./mvnw compile exec:java -Dexec.mainClass=ReceiveLogsTopic -Dexec.args="#"

# To receive all logs from the facility "kern":
./mvnw compile exec:java -Dexec.mainClass=ReceiveLogsTopic -Dexec.args="kern.*"

# Or if you want to hear only about "critical" logs:
./mvnw compile exec:java -Dexec.mainClass=ReceiveLogsTopic -Dexec.args="*.critical"

# You can create multiple bindings:
./mvnw compile exec:java -Dexec.mainClass=ReceiveLogsTopic -Dexec.args="kern.* *.critical"

# terminal tab 2
# And to emit a log with a routing key "kern.critical" type:
./mvnw compile exec:java -Dexec.mainClass=EmitLogTopic -Dexec.args="kern.critical A critical kernel error"
```

#### Tutorial six: RPC

```shell
# terminal tab 1
./mvnw compile exec:java -Dexec.mainClass=RPCServer

# terminal tab 2
./mvnw compile exec:java -Dexec.mainClass=RPCClient
```

#### Tutorial seven: Publisher Confirms

```shell
./mvnw compile exec:java -Dexec.mainClass=PublisherConfirms
```
