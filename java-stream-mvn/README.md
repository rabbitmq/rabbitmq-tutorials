# Java code for RabbitMQ tutorials

Here you can find Java code examples from [RabbitMQ tutorials](https://www.rabbitmq.com/getstarted.html).

To successfully use the examples you will need a running RabbitMQ server with the [stream plugin enabled](https://www.rabbitmq.com/docs/stream#enabling-plugin).

See [First Application With RabbitMQ Streams](https://www.rabbitmq.com/blog/2021/07/19/rabbitmq-streams-first-application), [Stream plugin documentation](https://www.rabbitmq.com/docs/stream) and [how to preconfigure plugins](https://www.rabbitmq.com/docs/plugins#enabled-plugins-file).

## Requirements

These examples use the [`rabbitmq-stream-java-client`](https://github.com/rabbitmq/rabbitmq-stream-java-client) client library.
This example uses Maven to manage dependencies.

## Code

Code examples are executed via the Maven wrapper `./mvnw`.
Remove the `-q` flag to get more information in case an example does not behave as expected.

[Tutorial one: "Hello World!"](https://www.rabbitmq.com/tutorials/tutorial-one-java-stream.html):

```
./mvnw -q compile exec:java -Dexec.mainClass="Send"
./mvnw -q compile exec:java -Dexec.mainClass="Receive"
```

To learn more, see [`rabbitmq/rabbitmq-stream-java-client`](https://github.com/rabbitmq/rabbitmq-stream-java-client).
