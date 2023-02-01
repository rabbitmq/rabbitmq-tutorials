# Java code for RabbitMQ tutorials

Here you can find the Java code examples from [RabbitMQ
tutorials](https://www.rabbitmq.com/getstarted.html).

To successfully use the examples you will need a RabbitMQ node running locally.

## Requirements

### Linux

- Note the source files are symbolic links to the [java](https://github.com/rabbitmq/rabbitmq-tutorials/tree/main/java) directory.

### Windows

- Run pull-source-files.bat to replace symbolic link to the actual source file.

```shell
.\pull-source-files.bat
```

## Code

#### [Tutorial one: "Hello World!"](https://www.rabbitmq.com/tutorials/tutorial-one-java.html):

```shell
# terminal tab 1
gradle -Pmain=Recv run

# terminal tab 2
gradle -Pmain=Send run
```

#### [Tutorial two: Work Queues](https://www.rabbitmq.com/tutorials/tutorial-two-java.html):

```shell
# terminal tab 1
gradle -Pmain=Worker run
gradle -Pmain=Worker run

# terminal tab 2
gradle -Pmain=NewTask run --args "First Message"
gradle -Pmain=NewTask run --args "Second Message"
gradle -Pmain=NewTask run --args "Third Message"
gradle -Pmain=NewTask run --args "Fourth Message"
gradle -Pmain=NewTask run --args "Fifth Message"
```

#### [Tutorial three: Publish/Subscribe](https://www.rabbitmq.com/tutorials/tutorial-three-java.html)

```shell
# terminal tab 1
gradle -Pmain=ReceiveLogs run

# terminal tab 2
gradle -Pmain=EmitLog run
```

#### [Tutorial four: Routing](https://www.rabbitmq.com/tutorials/tutorial-four-java.html)

```shell
# terminal tab 1
gradle -Pmain=ReceiveLogsDirect run --args "warning error"

# terminal tab 2
gradle -Pmain=ReceiveLogsDirect run --args "info warning error"

# terminal tab 3
gradle -Pmain=EmitLogDirect run --args "error Run. Run. Or it will explode"
```

#### [Tutorial five: Topics](https://www.rabbitmq.com/tutorials/tutorial-five-java.html)

```shell
# To receive all the logs:
gradle -Pmain=ReceiveLogsTopic run --args "#"

# To receive all logs from the facility "kern":
gradle -Pmain=ReceiveLogsTopic run --args "kern.*"

# Or if you want to hear only about "critical" logs:
gradle -Pmain=ReceiveLogsTopic run --args "*.critical"

# You can create multiple bindings:
gradle -Pmain=ReceiveLogsTopic run --args "kern.* *.critical"

# And to emit a log with a routing key "kern.critical" type:
gradle -Pmain=EmitLogTopic run --args "kern.critical A critical kernel error"
```

#### [Tutorial six: RPC](https://www.rabbitmq.com/tutorials/tutorial-six-java.html)

```shell
# Our RPC service is now ready. We can start the server:
gradle -Pmain=RPCServer run

# To request a fibonacci number run the client:
gradle -Pmain=RPCClient run
```

#### [Tutorial seven: Publisher Confirms](https://www.rabbitmq.com/tutorials/tutorial-seven-java.html)

```shell
#
gradle -Pmain=PublisherConfirms run
```
