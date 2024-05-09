# Java code for RabbitMQ tutorials

Here you can find Java code examples from [RabbitMQ tutorials](https://www.rabbitmq.com/getstarted.html).

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
