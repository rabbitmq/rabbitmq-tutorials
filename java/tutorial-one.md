## RabbitMQ

RabbitMQ is a message broker. In essence, it accepts messages from
_producers_, and delivers them to _consumers_. In-between, it can
route, buffer, and persist the messages according to rules you give
it.

# "Hello World"

In this tutorial we'll install RabbitMQ, then write two programs in
Java; a producer that sends a single message, and a consumer that
receives messages.  We'll gloss over some of the detail in the Java
API, concentrating on this very simple thing just to get started.
It's a "Hello World" of messaging.

In the diagram below, "P" is our producer and "C" is our consumer. The
box in the middle is a queue -- a message buffer that RabbitMQ keeps
on behalf of the consumer.

**TODO** diagram

## Installing the RabbitMQ server

If you are using a debian-based system, you can just

    $ apt-get install rabbitmq-server

Otherwise, follow the [install
instructions](http://www.rabbitmq.com/install.html) for your platform
to get the RabbitMQ server running.

You can test that it's running by issuing

    $ rabbitmqctl status

which will either tell you about the running applications, e.g.,

    Status of node rabbit@example ...
    [{running_applications,[{os_mon,"CPO  CXC 138 46","2.2.5"},
                            {sasl,"SASL  CXC 138 11","2.1.9"},
                            {mnesia,"MNESIA  CXC 138 12","4.4.13"},
                            {stdlib,"ERTS  CXC 138 10","1.16.5"},
                            {kernel,"ERTS  CXC 138 10","2.13.5"}]},
     {nodes,[{disc,[rabbit@example]}]},
     {running_nodes,[rabbit@example]}]
    ...done.

in which case all is well, or tell you there's no RabbitMQ server
running and give you some diagnostic information.

## The Java client library

RabbitMQ speaks [AMQP](http://amqp.org/), which is an open,
general-purpose protocol for messaging. There are a number of clients
for AMQP in [many different
languages](http://www.delicious.com/alexisrichardson/rabbitmq+client). We'll
use the Java client provided by RabbitMQ.

Download the [client library
package](http://www.rabbitmq.com/java-client.html), and check its
signature as described. Unzip it into your working directory and grab
the JAR files from the unzipped directory:

    $ unzip rabbitmq-java-client-bin-*.zip
    $ cp rabbitmq-java-client-bin-*/*.jar ./

(The RabbitMQ Java client is also in the central Maven repository,
with the groupId `com.rabbitmq` and the artifactId `amqp-client`.)

Now we have the Java client and its dependencies, we can write some
code.

# Sending

We'll call our message sender `send` and our message receiver
`recv`.  The sender will connect to RabbitMQ, send a single message,
then exit.

In
[`send.java`](http://github.com/rabbitmq/rabbitmq-tutorials/blob/master/java/send.java),
we need some classes imported: (**TODO** line numbers)

    import com.rabbitmq.client.ConnectionFactory;
    import com.rabbitmq.client.Connection;
    import com.rabbitmq.client.Channel;
    import java.io.IOException;

then we can create a connection to the server:

    public class send {
      public static void main(String[] argv) {
          Connection conn = new ConnectionFactory().newConnection();

The connection abstracts the socket connection, and takes care of
protocol version negotiation and authentication and so on for us.
Next we create a channel, which is where most of the API for getting
things done resides:

          Channel chan = conn.createChannel();

To send, we declare a queue for us to send to, and publish a message
to the queue:

          chan.queueDeclare("hello", false, false, false, null);
          chan.basicPublish("", "hello", null, "Hello World!".getBytes());

Declaring a queue is idempotent; it will be created if it's doesn't
exist already. The message contents is a byte array, so you can encode
whatever you like there.

Lastly, we close the channel and the connection;

          conn.close();

Since many of these method calls can throw an `IOException`, we wrap
the whole thing in a `try...catch`.  [Here's the whole of the class](http://github.com/rabbitmq/rabbitmq-tutorials/blob/master/java/send.java).

## Consuming messages

That's it for our sender.  Our receiver is pushed messages from
RabbitMQ, so unlike the sender which publishes a single message, we'll
keep it running to listen for messages and print them out.

The code (in [`recv.java`](http://github.com/rabbitmq/rabbitmq-tutorials/blob/master/java/recv.java)) has almost the same imports as `send`:

    import com.rabbitmq.client.ConnectionFactory;
    import com.rabbitmq.client.Connection;
    import com.rabbitmq.client.Channel;
    import com.rabbitmq.client.QueueingConsumer;
    import java.io.IOException;

The extra `QueueingConsumer` is a class we'll use to buffer the
messages pushed to us by the server.

Setting up is the same as the sender; we open a connection and a
channel, and declare the queue from which we're going to consume.
Note this matches up with the queue `send` publishes to.

    public class recv {
      public static void main(String[] argv) {
        try {
          Connection conn = new ConnectionFactory().newConnection();
          Channel chan = conn.createChannel();
          chan.queueDeclare("hello", false, false, false, null);

We're about to tell the server to deliver us the messages from the
queue. Since it will push us messages asynchronously, we provide a
callback in the form of an object that will buffer the messages until
we're ready to use them. That is what `QueueingConsumer` does.

          QueueingConsumer consumer = new QueueingConsumer(chan);
          chan.basicConsume("hello", true, consumer);
          while (true) {
            QueueingConsumer.Delivery delivery = consumer.nextDelivery();
            System.out.println(new String(delivery.getBody()));
          }

`QueueingConsumer.nextDelivery()` blocks until another message has
been delivered from the server.

The rest is just closing the `try...catch` -- [here's the whole class](http://github.com/rabbitmq/rabbitmq-tutorials/blob/master/java/recv.java).

You can compile both of these with just the RabbitMQ java client on
the classpath:

    $ javac -cp rabbitmq-client.jar send.java recv.java

To run them, you'll need `rabbitmq-client.jar` and its dependencies on
the classpath.  In one shell, run the receiver:

    shell1$ java -cp .:commons-io-1.2.jar:commons-cli-1.1.jar:rabbitmq-client.jar recv

and in another, run the sender:

    shell2$ java -cp .:commons-io-1.2.jar:commons-cli-1.1.jar:rabbitmq-client.jar send

The receiver will print the messages it gets from the sender via
RabbitMQ. If you run `send` before starting `recv`, you'll
notice that RabbitMQ holds onto the message until `recv` connects and
consumes from the queue.

Hello World!
