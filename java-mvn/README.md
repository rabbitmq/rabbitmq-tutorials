# Java code for RabbitMQ tutorials

Here you can find the Java code examples from [RabbitMQ
tutorials](https://www.rabbitmq.com/getstarted.html).

To successfully use the examples you will need a RabbitMQ node running locally.

You can easily set this up by [installing RabbitMQ](https://www.rabbitmq.com/docs/download).

## Requirements

### Linux

- Note the source files are symbolic links to the [java](https://github.com/rabbitmq/rabbitmq-tutorials/tree/main/java) directory.

### Windows

- Run pull-source-files.bat to replace symbolic link to the actual source file.

```shell
./pull-source-files.bat
```

## Code

#### [Tutorial one: "Hello World!"](https://www.rabbitmq.com/tutorials/tutorial-one-java.html):

```shell
# terminal tab 1
./mvnw compile exec:java -D"exec.mainClass=Recv"

# terminal tab 2
./mvnw compile exec:java -D"exec.mainClass=Send"
```

#### [Tutorial two: Work Queues](https://www.rabbitmq.com/tutorials/tutorial-two-java.html):

```shell
# terminal tab 1
./mvnw compile exec:java -D"exec.mainClass=Worker"

# terminal tab 2
./mvnw compile exec:java -D"exec.mainClass=Worker"

# terminal tab 3
./mvnw compile exec:java -D"exec.mainClass=NewTask" -D"exec.args='First Message'"
./mvnw compile exec:java -D"exec.mainClass=NewTask" -D"exec.args='Second Message'"
./mvnw compile exec:java -D"exec.mainClass=NewTask" -D"exec.args='Third Message'"
./mvnw compile exec:java -D"exec.mainClass=NewTask" -D"exec.args='Fourth Message'"
./mvnw compile exec:java -D"exec.mainClass=NewTask" -D"exec.args='Fifth Message'"
```

#### [Tutorial three: Publish/Subscribe](https://www.rabbitmq.com/tutorials/tutorial-three-java.html)

```shell
# terminal tab 1
./mvnw compile exec:java -D"exec.mainClass=ReceiveLogs"

# terminal tab 2
./mvnw compile exec:java -D"exec.mainClass=ReceiveLogs"

# terminal tab 3
./mvnw compile exec:java -D"exec.mainClass=EmitLog"
```

#### [Tutorial four: Routing](https://www.rabbitmq.com/tutorials/tutorial-four-java.html)

```shell
# terminal tab 1
./mvnw compile exec:java -D"exec.mainClass=ReceiveLogsDirect" -D"exec.args=warning error"

# terminal tab 2
./mvnw compile exec:java -D"exec.mainClass=ReceiveLogsDirect" -D"exec.args=info warning error"

# terminal tab 3
./mvnw compile exec:java -D"exec.mainClass=EmitLogDirect" -D"exec.args=info 'Run. Run. Or it will explode'"
./mvnw compile exec:java -D"exec.mainClass=EmitLogDirect" -D"exec.args=warning 'Run. Run. Or it will explode'"
./mvnw compile exec:java -D"exec.mainClass=EmitLogDirect" -D"exec.args=error 'Run. Run. Or it will explode'"
```

#### [Tutorial five: Topics](https://www.rabbitmq.com/tutorials/tutorial-five-java.html)

```shell
# terminal tab 1
# To receive all the logs:
./mvnw compile exec:java -D"exec.mainClass=ReceiveLogsTopic" -D"exec.args=#"

# To receive all logs from the facility "kern":
./mvnw compile exec:java -D"exec.mainClass=ReceiveLogsTopic" -D"exec.args=kern.*"

# Or if you want to hear only about "critical" logs:
./mvnw compile exec:java -D"exec.mainClass=ReceiveLogsTopic" -D"exec.args=*.critical"

# You can create multiple bindings:
./mvnw compile exec:java -D"exec.mainClass=ReceiveLogsTopic" -D"exec.args=kern.* *.critical"

# terminal tab 2
# And to emit a log with a routing key "kern.critical" type:
./mvnw compile exec:java -D"exec.mainClass=EmitLogTopic" -D"exec.args=kern.critical A critical kernel error"
```

#### [Tutorial six: RPC](https://www.rabbitmq.com/tutorials/tutorial-six-java.html)

```shell
# terminal tab 1
# Our RPC service is now ready. We can start the server:
./mvnw compile exec:java -D"exec.mainClass=RPCServer"

# terminal tab 2
# To request a fibonacci number run the client:
./mvnw compile exec:java -D"exec.mainClass=RPCClient"
```

#### [Tutorial seven: Publisher Confirms](https://www.rabbitmq.com/tutorials/tutorial-seven-java.html)

```shell
# terminal tab 1
./mvnw compile exec:java -D"exec.mainClass=PublisherConfirms"
```
