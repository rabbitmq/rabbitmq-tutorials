# Java code for RabbitMQ tutorials

Here you can find the Java code examples from [RabbitMQ
tutorials](http://www.rabbitmq.com/getstarted.html).

To successfully use the examples you will need a running RabbitMQ server.

## Requirements

You'll need to download the RabbitMQ 
[java client library package](http://www.rabbitmq.com/java-client.html),
and check its signature as described there. 
Unzip it into your working directory and ensure the JAR files from the 
unzipped directory are placed in your working directory:

    $ unzip rabbitmq-java-client-bin-*.zip
    $ cp rabbitmq-java-client-bin-*/*.jar ./

To compile you only need the Rabbitmq java client jar on the classpath.

To run them you'll need all the dependencies, see examples below. 

Note: If you're on Windows, 
use a semicolon instead of a colon to separate items in the classpath.

> You can set an environment variable for the jar files on the classpath e.g.
>
>      $ export CP=.:commons-io-1.2.jar:commons-cli-1.1.jar:rabbitmq-client.jar
>      $ java -cp $CP Send
>
> or on Windows:
>
>      > set CP=.;commons-io-1.2.jar;commons-cli-1.1.jar;rabbitmq-client.jar
>      > java -cp %CP% Send

## Code

[Tutorial one: "Hello World!"](http://www.rabbitmq.com/tutorial-one-java.html):

    $ javac -cp rabbitmq-client.jar Send.java Recv.java

    $ java -cp .:commons-io-1.2.jar:commons-cli-1.1.jar:rabbitmq-client.jar Send
    $ java -cp .:commons-io-1.2.jar:commons-cli-1.1.jar:rabbitmq-client.jar Recv

[Tutorial two: Work Queues](http://www.rabbitmq.com/tutorial-two-java.html):

    $ javac -cp rabbitmq-client.jar NewTask.java Worker.java

    $ java -cp $CP NewTask
    $ java -cp $CP Worker

[Tutorial three: Publish/Subscribe](http://www.rabbitmq.com/tutorial-three-java.html)

    $ javac -cp rabbitmq-client.jar EmitLog.java ReceiveLogs.java
    
    $ java -cp $CP EmitLog
    $ java -cp $CP ReceiveLogs


