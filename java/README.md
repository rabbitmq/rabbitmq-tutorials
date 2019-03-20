# Java code for RabbitMQ tutorials

Here you can find the Java code examples from [RabbitMQ
tutorials](https://www.rabbitmq.com/getstarted.html).

To successfully use the examples you will need a running RabbitMQ server.

## Requirements

You'll need to download the following JAR files
from Maven Central:
    
 * [RabbitMQ Java Client](http://central.maven.org/maven2/com/rabbitmq/amqp-client/4.0.2/amqp-client-4.0.2.jar)
 * [SLF4J API](http://central.maven.org/maven2/org/slf4j/slf4j-api/1.7.21/slf4j-api-1.7.21.jar)
 * [SLF4J Simple](http://central.maven.org/maven2/org/slf4j/slf4j-simple/1.7.22/slf4j-simple-1.7.22.jar)

Copy those files in your working directory, along the tutorials Java files.

To compile you only need the Rabbitmq Java Client jar on the classpath.

To run them you'll need all the dependencies, see examples below.

Note: If you're on Windows,
use a semicolon instead of a colon to separate items in the classpath.

> You can set an environment variable for the jar files on the classpath e.g.
>
>      $ export CP=.:amqp-client-4.0.2.jar:slf4j-api-1.7.21.jar:slf4j-simple-1.7.22.jar
>      $ java -cp $CP Send
>
> or on Windows:
>
>      > set CP=.;amqp-client-4.0.2.jar;slf4j-api-1.7.21.jar;slf4j-simple-1.7.22.jar
>      > java -cp %CP% Send

## Code

[Tutorial one: "Hello World!"](https://www.rabbitmq.com/tutorial-one-java.html):

    $ javac -cp amqp-client-4.0.2.jar Send.java Recv.java

    $ java -cp .:amqp-client-4.0.2.jar:slf4j-api-1.7.21.jar:slf4j-simple-1.7.22.jar Send
    $ java -cp .:amqp-client-4.0.2.jar:slf4j-api-1.7.21.jar:slf4j-simple-1.7.22.jar Recv

[Tutorial two: Work Queues](https://www.rabbitmq.com/tutorial-two-java.html):

    $ javac -cp amqp-client-4.0.2.jar NewTask.java Worker.java

    $ java -cp $CP NewTask
    $ java -cp $CP Worker

[Tutorial three: Publish/Subscribe](https://www.rabbitmq.com/tutorial-three-java.html)

    $ javac -cp amqp-client-4.0.2.jar EmitLog.java ReceiveLogs.java

    $ java -cp $CP ReceiveLogs
    $ java -cp $CP EmitLog


