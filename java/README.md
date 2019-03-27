# Java code for RabbitMQ tutorials

Here you can find the Java code examples from [RabbitMQ
tutorials](https://www.rabbitmq.com/getstarted.html).

To successfully use the examples you will need a RabbitMQ node running locally.

## Requirements

You'll need to download the following JAR files
from Maven Central:

 * [RabbitMQ Java Client](https://repo1.maven.org/maven2/com/rabbitmq/amqp-client/5.6.0//amqp-client-5.6.0.jar)
 * [SLF4J API](https://repo1.maven.org/maven2/org/slf4j/slf4j-api/1.7.25/slf4j-api-1.7.25.jar)
 * [SLF4J Simple](https://repo1.maven.org/maven2/org/slf4j/slf4j-simple/1.7.25/slf4j-simple-1.7.25.jar)

For example, with `wget`:

``` shell
wget https://repo1.maven.org/maven2/com/rabbitmq/amqp-client/5.6.0/amqp-client-5.6.0.jar
wget https://repo1.maven.org/maven2/org/slf4j/slf4j-api/1.7.25/slf4j-api-1.7.25.jar
wget https://repo1.maven.org/maven2/org/slf4j/slf4j-simple/1.7.25/slf4j-simple-1.7.25.jar
```

Copy those files in your working directory, along the tutorials Java files.

To compile you only need the Rabbitmq Java client jar on the classpath.
To run them you'll need all the dependencies, see examples below.

You can set an environment variable for the jar files on the classpath e.g.

```
export CP=.:amqp-client-5.6.0.jar:slf4j-api-1.7.25.jar:slf4j-simple-1.7.25.jar
java -cp $CP Send
```

On Windows, use a semicolon instead of a colon to separate items in the classpath:

```
set CP=.;amqp-client-5.6.0.jar;slf4j-api-1.7.25.jar;slf4j-simple-1.7.25.jar
java -cp %CP% Send
```

## Code

[Tutorial one: "Hello World!"](https://www.rabbitmq.com/tutorial-one-java.html):

```
javac -cp amqp-client-5.6.0.jar Send.java Recv.java

# terminal tab 1
java -cp .:amqp-client-5.6.0.jar:slf4j-api-1.7.25.jar:slf4j-simple-1.7.25.jar Send

# terminal tab 2
java -cp .:amqp-client-5.6.0.jar:slf4j-api-1.7.25.jar:slf4j-simple-1.7.25.jar Recv
```

[Tutorial two: Work Queues](https://www.rabbitmq.com/tutorial-two-java.html):

```
javac -cp amqp-client-5.6.0.jar NewTask.java Worker.java

# terminal tab 1
java -cp $CP NewTask

# terminal tab 2
java -cp $CP Worker
```

[Tutorial three: Publish/Subscribe](https://www.rabbitmq.com/tutorial-three-java.html)

``` shell
javac -cp amqp-client-5.6.0.jar EmitLog.java ReceiveLogs.java

# terminal tab 1
java -cp $CP ReceiveLogs

# terminal tab 2
java -cp $CP EmitLog
```
