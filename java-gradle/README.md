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
./gradlew -Pmain=Recv run

# terminal tab 2
./gradlew -Pmain=Send run
```

#### [Tutorial two: Work Queues](https://www.rabbitmq.com/tutorials/tutorial-two-java.html):

```shell
# terminal tab 1
./gradlew -Pmain=Worker run

# terminal tab 2
./gradlew -Pmain=Worker run

# terminal tab 3
./gradlew -Pmain=NewTask run --args "First Message"
./gradlew -Pmain=NewTask run --args "Second Message"
./gradlew -Pmain=NewTask run --args "Third Message"
./gradlew -Pmain=NewTask run --args "Fourth Message"
./gradlew -Pmain=NewTask run --args "Fifth Message"
```

#### [Tutorial three: Publish/Subscribe](https://www.rabbitmq.com/tutorials/tutorial-three-java.html)

```shell
# terminal tab 1
./gradlew -Pmain=ReceiveLogs run

# terminal tab 2
./gradlew -Pmain=ReceiveLogs run

# terminal tab 3
./gradlew -Pmain=EmitLog run
```

#### [Tutorial four: Routing](https://www.rabbitmq.com/tutorials/tutorial-four-java.html)

```shell
# terminal tab 1
./gradlew -Pmain=ReceiveLogsDirect run --args "warning error"

# terminal tab 2
./gradlew -Pmain=ReceiveLogsDirect run --args "info warning error"

# terminal tab 3
./gradlew -Pmain=EmitLogDirect run --args "info 'Run. Run. Or it will explode'"
./gradlew -Pmain=EmitLogDirect run --args "warning 'Run. Run. Or it will explode'"
./gradlew -Pmain=EmitLogDirect run --args "error 'Run. Run. Or it will explode'"
```

#### [Tutorial five: Topics](https://www.rabbitmq.com/tutorials/tutorial-five-java.html)

```shell
# terminal tab 1
# To receive all the logs:
./gradlew -Pmain=ReceiveLogsTopic run --args "#"

# To receive all logs from the facility "kern":
./gradlew -Pmain=ReceiveLogsTopic run --args "kern.*"

# Or if you want to hear only about "critical" logs:
./gradlew -Pmain=ReceiveLogsTopic run --args "*.critical"

# You can create multiple bindings:
./gradlew -Pmain=ReceiveLogsTopic run --args "kern.* *.critical"

# terminal tab 2
# And to emit a log with a routing key "kern.critical" type:
./gradlew -Pmain=EmitLogTopic run --args "kern.critical A critical kernel error"
```

#### [Tutorial six: RPC](https://www.rabbitmq.com/tutorials/tutorial-six-java.html)

```shell
# terminal tab 1
# Our RPC service is now ready. We can start the server:
./gradlew -Pmain=RPCServer run

# terminal tab 2
# To request a fibonacci number run the client:
./gradlew -Pmain=RPCClient run
```

#### [Tutorial seven: Publisher Confirms](https://www.rabbitmq.com/tutorials/tutorial-seven-java.html)

```shell
# terminal tab 1
./gradlew -Pmain=PublisherConfirms run
```
