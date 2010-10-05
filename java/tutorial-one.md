
## Introduction

RabbitMQ is a message broker. In essence, it accepts messages from
_producers_, and delivers them to _consumers_. In-between, it can
route, buffer, and persist the messages according to rules you give
it.

# "Hello World"

In this part of the tutorial we'll write two programs in Java; a
producer that sends a single message, and a consumer that receives
messages and prints them out.  We'll gloss over some of the detail in
the Java API, concentrating on this very simple thing just to get
started.  It's a "Hello World" of messaging.

In the diagram below, "P" is our producer and "C" is our consumer. The
box in the middle is a queue -- a message buffer that RabbitMQ keeps
on behalf of the consumer.


<center><div class="dot_bitmap">
<img src="http://github.com/rabbitmq/rabbitmq-tutorials/raw/master/_img/3314f4be42ba3db9b161e564def3daca.png" alt="Dot graph" width="392" height="59" />
</div></center>


> #### The Java client library
>
> RabbitMQ speaks [AMQP](http://amqp.org/), which is an open,
> general-purpose protocol for messaging. There are a number of clients
> for AMQP in [many different
> languages](http://www.delicious.com/alexisrichardson/rabbitmq+client). We'll
> use the Java client provided by RabbitMQ.
>
> Download the [client library
> package](http://www.rabbitmq.com/java-client.html), and check its
> signature as described. Unzip it into your working directory and grab
> the JAR files from the unzipped directory:
>
>    $ unzip rabbitmq-java-client-bin-*.zip
>    $ cp rabbitmq-java-client-bin-*/*.jar ./
>
> (The RabbitMQ Java client is also in the central Maven repository,
> with the groupId `com.rabbitmq` and the artifactId `amqp-client`.)

Now we have the Java client and its dependencies, we can write some
code.

### Sending


<center><div class="dot_bitmap">
<img src="http://github.com/rabbitmq/rabbitmq-tutorials/raw/master/_img/28a5099cc807b687e36772091edcf740.png" alt="Dot graph" width="223" height="48" />
</div></center>


We'll call our message sender `send` and our message receiver
`recv`.  The sender will connect to RabbitMQ, send a single message,
then exit.

In
[`send.java`](http://github.com/rabbitmq/rabbitmq-tutorials/blob/master/java/send.java),
we need some classes imported:

<table class="highlighttable"><tr><td class="linenos"><div class="linenodiv"><pre><code class="java">1
2
3
4</code></pre></div></td><td class="code"><div class="highlight"><pre><span class="kn">import</span> <span class="nn">com.rabbitmq.client.ConnectionFactory</span><span class="o">;</span>
<span class="kn">import</span> <span class="nn">com.rabbitmq.client.Connection</span><span class="o">;</span>
<span class="kn">import</span> <span class="nn">com.rabbitmq.client.Channel</span><span class="o">;</span>
<span class="kn">import</span> <span class="nn">java.io.IOException</span><span class="o">;</span>
</pre></div>
</td></tr></table>

then we can create a connection to the server:

<table class="highlighttable"><tr><td class="linenos"><div class="linenodiv"><pre><code class="java"> 6
 7
 8
 9
10</code></pre></div></td><td class="code"><div class="highlight"><pre><span class="kd">public</span> <span class="kd">class</span> <span class="nc">send</span> <span class="o">{</span>
  <span class="kd">public</span> <span class="kd">static</span> <span class="kt">void</span> <span class="nf">main</span><span class="o">(</span><span class="n">String</span><span class="o">[]</span> <span class="n">argv</span><span class="o">)</span> <span class="o">{</span>
    <span class="k">try</span> <span class="o">{</span>
      <span class="n">Connection</span> <span class="n">conn</span> <span class="o">=</span> <span class="k">new</span> <span class="n">ConnectionFactory</span><span class="o">().</span><span class="na">newConnection</span><span class="o">();</span>
      <span class="n">Channel</span> <span class="n">chan</span> <span class="o">=</span> <span class="n">conn</span><span class="o">.</span><span class="na">createChannel</span><span class="o">();</span>
</pre></div>
</td></tr></table>

The connection abstracts the socket connection, and takes care of
protocol version negotiation and authentication and so on for us.
Next we create a channel, which is where most of the API for getting
things done resides.

To send, we must declare a queue for us to send to; then we can publish a message
to the queue:

<table class="highlighttable"><tr><td class="linenos"><div class="linenodiv"><pre><code class="java">11
12</code></pre></div></td><td class="code"><div class="highlight"><pre>      <span class="n">chan</span><span class="o">.</span><span class="na">queueDeclare</span><span class="o">(</span><span class="s">&quot;hello&quot;</span><span class="o">,</span> <span class="kc">false</span><span class="o">,</span> <span class="kc">false</span><span class="o">,</span> <span class="kc">false</span><span class="o">,</span> <span class="kc">null</span><span class="o">);</span>
      <span class="n">chan</span><span class="o">.</span><span class="na">basicPublish</span><span class="o">(</span><span class="s">&quot;&quot;</span><span class="o">,</span> <span class="s">&quot;hello&quot;</span><span class="o">,</span> <span class="kc">null</span><span class="o">,</span> <span class="s">&quot;Hello World!&quot;</span><span class="o">.</span><span class="na">getBytes</span><span class="o">());</span>
</pre></div>
</td></tr></table>

Declaring a queue is idempotent; it will be created if it's doesn't
exist already. The message contents is a byte array, so you can encode
whatever you like there.

Lastly, we close the channel and the connection;

<table class="highlighttable"><tr><td class="linenos"><div class="linenodiv"><pre><code class="java">13</code></pre></div></td><td class="code"><div class="highlight"><pre>      <span class="n">conn</span><span class="o">.</span><span class="na">close</span><span class="o">();</span>
</pre></div>
</td></tr></table>

Since many of these method calls can throw an `IOException`, we wrap
the whole thing in a `try...catch`.  [Here's the whole of the class](http://github.com/rabbitmq/rabbitmq-tutorials/blob/master/java/send.java).

### Receiving

That's it for our sender.  Our receiver is pushed messages from
RabbitMQ, so unlike the sender which publishes a single message, we'll
keep it running to listen for messages and print them out.


<center><div class="dot_bitmap">
<img src="http://github.com/rabbitmq/rabbitmq-tutorials/raw/master/_img/39d6d05c8bd0aaf7d7993ada5a785ae2.png" alt="Dot graph" width="223" height="48" />
</div></center>


The code (in [`recv.java`](http://github.com/rabbitmq/rabbitmq-tutorials/blob/master/java/recv.java)) has almost the same imports as `send`:

<table class="highlighttable"><tr><td class="linenos"><div class="linenodiv"><pre><code class="java">1
2
3
4
5</code></pre></div></td><td class="code"><div class="highlight"><pre><span class="kn">import</span> <span class="nn">com.rabbitmq.client.ConnectionFactory</span><span class="o">;</span>
<span class="kn">import</span> <span class="nn">com.rabbitmq.client.Connection</span><span class="o">;</span>
<span class="kn">import</span> <span class="nn">com.rabbitmq.client.Channel</span><span class="o">;</span>
<span class="kn">import</span> <span class="nn">com.rabbitmq.client.QueueingConsumer</span><span class="o">;</span>
<span class="kn">import</span> <span class="nn">java.io.IOException</span><span class="o">;</span>
</pre></div>
</td></tr></table>

The extra `QueueingConsumer` is a class we'll use to buffer the
messages pushed to us by the server.

Setting up is the same as the sender; we open a connection and a
channel, and declare the queue from which we're going to consume.
Note this matches up with the queue `send` publishes to.

<table class="highlighttable"><tr><td class="linenos"><div class="linenodiv"><pre><code class="java"> 7
 8
 9
10
11
12</code></pre></div></td><td class="code"><div class="highlight"><pre><span class="kd">public</span> <span class="kd">class</span> <span class="nc">recv</span> <span class="o">{</span>
  <span class="kd">public</span> <span class="kd">static</span> <span class="kt">void</span> <span class="nf">main</span><span class="o">(</span><span class="n">String</span><span class="o">[]</span> <span class="n">argv</span><span class="o">)</span> <span class="o">{</span>
    <span class="k">try</span> <span class="o">{</span>
      <span class="n">Connection</span> <span class="n">conn</span> <span class="o">=</span> <span class="k">new</span> <span class="n">ConnectionFactory</span><span class="o">().</span><span class="na">newConnection</span><span class="o">();</span>
      <span class="n">Channel</span> <span class="n">chan</span> <span class="o">=</span> <span class="n">conn</span><span class="o">.</span><span class="na">createChannel</span><span class="o">();</span>
      <span class="n">chan</span><span class="o">.</span><span class="na">queueDeclare</span><span class="o">(</span><span class="s">&quot;hello&quot;</span><span class="o">,</span> <span class="kc">false</span><span class="o">,</span> <span class="kc">false</span><span class="o">,</span> <span class="kc">false</span><span class="o">,</span> <span class="kc">null</span><span class="o">);</span>
</pre></div>
</td></tr></table>

Note that we declare the queue here, as well. Because we might start
the receiver before the sender, we want to make sure the queue exists
before we try to consumer messages from it.

We're about to tell the server to deliver us the messages from the
queue. Since it will push us messages asynchronously, we provide a
callback in the form of an object that will buffer the messages until
we're ready to use them. That is what `QueueingConsumer` does.

<table class="highlighttable"><tr><td class="linenos"><div class="linenodiv"><pre><code class="java">13
14
15
16
17
18</code></pre></div></td><td class="code"><div class="highlight"><pre>      <span class="n">QueueingConsumer</span> <span class="n">consumer</span> <span class="o">=</span> <span class="k">new</span> <span class="n">QueueingConsumer</span><span class="o">(</span><span class="n">chan</span><span class="o">);</span>
      <span class="n">chan</span><span class="o">.</span><span class="na">basicConsume</span><span class="o">(</span><span class="s">&quot;hello&quot;</span><span class="o">,</span> <span class="kc">true</span><span class="o">,</span> <span class="n">consumer</span><span class="o">);</span>
      <span class="k">while</span> <span class="o">(</span><span class="kc">true</span><span class="o">)</span> <span class="o">{</span>
        <span class="n">QueueingConsumer</span><span class="o">.</span><span class="na">Delivery</span> <span class="n">delivery</span> <span class="o">=</span> <span class="n">consumer</span><span class="o">.</span><span class="na">nextDelivery</span><span class="o">();</span>
        <span class="n">System</span><span class="o">.</span><span class="na">out</span><span class="o">.</span><span class="na">println</span><span class="o">(</span><span class="k">new</span> <span class="n">String</span><span class="o">(</span><span class="n">delivery</span><span class="o">.</span><span class="na">getBody</span><span class="o">()));</span>
      <span class="o">}</span>
</pre></div>
</td></tr></table>

`QueueingConsumer.nextDelivery()` blocks until another message has
been delivered from the server.

The rest is just closing the `try...catch` -- [here's the whole class](http://github.com/rabbitmq/rabbitmq-tutorials/blob/master/java/recv.java).

### Putting it all together

You can compile both of these with just the RabbitMQ java client on
the classpath:

    $ javac -cp rabbitmq-client.jar send.java recv.java

To run them, you'll need `rabbitmq-client.jar` and its dependencies on
the classpath.  In a terminal, run the sender:

    $ java -cp .:commons-io-1.2.jar:commons-cli-1.1.jar:rabbitmq-client.jar send

then, run the sender:

    $ java -cp .:commons-io-1.2.jar:commons-cli-1.1.jar:rabbitmq-client.jar recv

The receiver will print the message it gets from the sender via
RabbitMQ. The receiver will keep running, waiting for messages; try running
the sender from another terminal.

Hello World!

