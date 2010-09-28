
Learning RabbitMQ, part 1 ("Hello world!")
==========================================



<center><div class="dot_bitmap">
<img src="http://github.com/rabbitmq/rabbitmq-tutorials/raw/master/python/_img/f102830d0b42138245d6a95010dad710.png" alt="Dot graph" width="384" height="59" />
</div></center>




Throughout this tutorial, we'll teach you the basic concepts required for
creating RabbitMQ applications. The tutorial will be illustrated with
code snippets written in [Python](http://www.python.org/). But don't worry if
you don't know this language - the core ideas are the same for other languages.

This tutorial consists of three parts:

 * First we're going to write the simplest possible "Hello World" example.
 * Next we'll try to use Rabbit as a simple "Work queue" server.
 * Finally, we'll discuss the "Publish-subscribe" pattern.

You need to have RabbitMQ server installed to go throught this tutorial.
If you haven't installed it yet you can follow the [installation instructions](http://www.rabbitmq.com/install.html). You can tell RabbitMQ is installed by checking if the tcp/ip port 5672 is opened. For example if you have installed RabbitMQ you should see something like:

    $ nc -vz localhost 5672
    localhost [127.0.0.1] 5672 (tcp/amqp) open



> #### Where to get help
>
> If you're having trouble going through this tutorial you can post a message to
> [rabbitmq-discuss mailing list](https://lists.rabbitmq.com/cgi-bin/mailman/listinfo/rabbitmq-discuss).


Introduction
------------

RabbitMQ is a message broker. The principle idea is pretty simple: it accepts
and forwards messages. You can think about it as a post office: when you send
mail to the post box and you're pretty sure that mr postman will eventually
deliver the mail to your recipient. Using this metaphor RabbitMQ is a post box,
post office and a mailman.

The major difference between RabbitMQ and the post office is the fact that it
doesn't deal with the paper, instead it accepts, stores and forwards binary
blobs of data - _messages_.

RabbitMQ uses a weird jargon, but it's simple once you'll get it. For example:

 * _Producing_ means nothing more than sending. A program that sends messages
   is a _producer_.

    
<center><div class="dot_bitmap">
<img src="http://github.com/rabbitmq/rabbitmq-tutorials/raw/master/python/_img/5a5fa54ed009ea0185a5f0131d1faff6.png" alt="Dot graph" width="41" height="29" />
</div></center>


 * _A queue_ is the name for a mailbox. It lives inside RabbitMQ.
    
<center><div class="dot_bitmap">
<img src="http://github.com/rabbitmq/rabbitmq-tutorials/raw/master/python/_img/9bdb70c65ab8b2aa1f6b0b85c2931a54.png" alt="Dot graph" width="72" height="29" />
</div></center>


 * _Consuming_ has a simmilar meaning to receiving. _Consumer_ is a program
   that mostly waits to receive messages.

    
<center><div class="dot_bitmap">
<img src="http://github.com/rabbitmq/rabbitmq-tutorials/raw/master/python/_img/c81197642d0b8ac05c7e7e89b65f2806.png" alt="Dot graph" width="41" height="29" />
</div></center>



Hello World!
------------

Our first "Hello world" won't be too complex - let's send a message, receive
it and print it on the screen. To do so we need two programs, well, one that
sends a message and one that receives and prints it.


Our overall design will look like:

<center><div class="dot_bitmap">
<img src="http://github.com/rabbitmq/rabbitmq-tutorials/raw/master/python/_img/3314f4be42ba3db9b161e564def3daca.png" alt="Dot graph" width="384" height="59" />
</div></center>




Creating a program
------------------



> #### RabbitMQ libraries
>
> RabbitMQ speaks AMQP protocol. To use Rabbit you'll need a library that
> understands the same protocol as Rabbit. There is a choice of libraries
> for almost every programming language. Python it's not different and there is
> a bunch of libraries to choose from:
> * [py-amqplib](http://barryp.org/software/py-amqplib/)
> * [txAMQP](https://launchpad.net/txamqp)
> * [pika](http://github.com/tonyg/pika)
>
> In this tutorial we're going to use `pika`. To install it you can use [`pip`](http://pip.openplans.org/) package management tool:
>
>    $ sudo pip install -e git://github.com/tonyg/pika.git#egg=pika
>
>If you don't have `pip`, you may want to install it.
>
> * On Ubuntu:
>
>        $ sudo apt-get install python-pip
>
> * On Debian:
>
>        $ sudo apt-get install python-setuptools
>        $ sudo easy_install pip


### Sending




<center><div class="dot_bitmap">
<img src="http://github.com/rabbitmq/rabbitmq-tutorials/raw/master/python/_img/28a5099cc807b687e36772091edcf740.png" alt="Dot graph" width="216" height="48" />
</div></center>



Our first program `send.py` will send a single message to the queue.
The first thing we need to do is to connect to RabbitMQ server.

<table class="highlighttable"><tr><td class="linenos"><pre><code class="python">1
2
3
4
5
6
7</code></pre></td><td class="code"><div class="highlight"><pre>    <span class="c">#!/usr/bin/env python</span>
    <span class="k">import</span> <span class="nn">pika</span>

    <span class="n">connection</span> <span class="o">=</span> <span class="n">pika</span><span class="o">.</span><span class="n">AsyncoreConnection</span><span class="p">(</span><span class="n">pika</span><span class="o">.</span><span class="n">ConnectionParameters</span><span class="p">(</span>
                   <span class="s">&#39;127.0.0.1&#39;</span><span class="p">,</span>
                   <span class="n">credentials</span> <span class="o">=</span> <span class="n">pika</span><span class="o">.</span><span class="n">PlainCredentials</span><span class="p">(</span><span class="s">&#39;guest&#39;</span><span class="p">,</span> <span class="s">&#39;guest&#39;</span><span class="p">))</span>
    <span class="n">channel</span> <span class="o">=</span> <span class="n">connection</span><span class="o">.</span><span class="n">channel</span><span class="p">()</span>
</pre></div>
</td></tr></table>


Whenever we send a message we need to make sure the recipient queue exists.
RabbitMQ will just trash the message if can't deliver it. So, we need to
create a queue to which the message will be delivered. Let's name this queue
_test_:

<table class="highlighttable"><tr><td class="linenos"><pre><code class="python">8</code></pre></td><td class="code"><div class="highlight"><pre>    <span class="n">channel</span><span class="o">.</span><span class="n">queue_declare</span><span class="p">(</span><span class="n">queue</span><span class="o">=</span><span class="s">&#39;test&#39;</span><span class="p">)</span>
</pre></div>
</td></tr></table>


At that point we're ready to send a message. Our first message will contain
a string _Hello World!_. We are going to send it to our _test_ queue:

<table class="highlighttable"><tr><td class="linenos"><pre><code class="python"> 9
10
11
12</code></pre></td><td class="code"><div class="highlight"><pre>    <span class="n">channel</span><span class="o">.</span><span class="n">basic_publish</span><span class="p">(</span><span class="n">exchange</span><span class="o">=</span><span class="s">&#39;&#39;</span><span class="p">,</span>
                          <span class="n">routing_key</span><span class="o">=</span><span class="s">&#39;test&#39;</span><span class="p">,</span>
                          <span class="n">body</span><span class="o">=</span><span class="s">&#39;Hello World!&#39;</span><span class="p">)</span>
    <span class="k">print</span> <span class="s">&quot; [x] Sent &#39;Hello World!&#39;&quot;</span>
</pre></div>
</td></tr></table>


[(full send.py source)](http://github.com/rabbitmq/rabbitmq-tutorials/blob/master/python/send.py)

### Receiving



<center><div class="dot_bitmap">
<img src="http://github.com/rabbitmq/rabbitmq-tutorials/raw/master/python/_img/39d6d05c8bd0aaf7d7993ada5a785ae2.png" alt="Dot graph" width="216" height="48" />
</div></center>



Our second program `receive.py` will receive messages from the queue and print
them on the screen. 

The code responsible for connecting to Rabbit is the same as in previous example.
You can copy the first 7 lines.

     # ... connection code is the same, copy first 7 lines from send.py ...

Just like before, in the beginning we must make sure that the
queue exists. Creating a queue using `queue_declare` is idempotent - you can
run the command as many times you'd like, and only one queue will be created.

<table class="highlighttable"><tr><td class="linenos"><pre><code class="python">8</code></pre></td><td class="code"><div class="highlight"><pre>    <span class="n">channel</span><span class="o">.</span><span class="n">queue_declare</span><span class="p">(</span><span class="n">queue</span><span class="o">=</span><span class="s">&#39;test&#39;</span><span class="p">)</span>
</pre></div>
</td></tr></table>


Receiving messages from the queue is a bit more complex. Whenever we receive
a message, a `callback` function is called. In our case
this function will print on the screen the contents of the message.

<table class="highlighttable"><tr><td class="linenos"><pre><code class="python"> 9
10</code></pre></td><td class="code"><div class="highlight"><pre>    <span class="k">def</span> <span class="nf">callback</span><span class="p">(</span><span class="n">ch</span><span class="p">,</span> <span class="n">method</span><span class="p">,</span> <span class="n">header</span><span class="p">,</span> <span class="n">body</span><span class="p">):</span>
        <span class="k">print</span> <span class="s">&quot; [x] Received </span><span class="si">%.20r</span><span class="s">&quot;</span> <span class="o">%</span> <span class="p">(</span><span class="n">body</span><span class="p">,)</span>
</pre></div>
</td></tr></table>


Next, we need to tell RabbitMQ that this particular callback function is
interested in messages from our _test_ queue:

<table class="highlighttable"><tr><td class="linenos"><pre><code class="python">11
12
13</code></pre></td><td class="code"><div class="highlight"><pre>    <span class="n">channel</span><span class="o">.</span><span class="n">basic_consume</span><span class="p">(</span><span class="n">callback</span><span class="p">,</span>
                          <span class="n">queue</span><span class="o">=</span><span class="s">&#39;test&#39;</span><span class="p">,</span>
                          <span class="n">no_ack</span><span class="o">=</span><span class="bp">True</span><span class="p">)</span>
</pre></div>
</td></tr></table>


And finally, we enter never-ending loop that waits for data and runs callbacks
whenever necessary.

<table class="highlighttable"><tr><td class="linenos"><pre><code class="python">14
15</code></pre></td><td class="code"><div class="highlight"><pre>    <span class="k">print</span> <span class="s">&#39; [*] Waiting for messages. To exit press CTRL+C&#39;</span>
    <span class="n">pika</span><span class="o">.</span><span class="n">asyncore_loop</span><span class="p">()</span>
</pre></div>
</td></tr></table>

[(full receive.py source)](http://github.com/rabbitmq/rabbitmq-tutorials/blob/master/python/receive.py)

### Putting it all together

Now we can try out our programs. First, let's send
a message using our `send.py` program:

    $ python send.py
     [x] Sent 'Hello World!'

Let's receive it:

    $ python receive.py
     [*] Waiting for messages. To exit press CTRL+C
     [x] Received 'Hello World!'

Hurray! We were able to send our first message through RabbitMQ. As you might
have noticed, the `receive.py` program haven't exited. It will stay ready to
receive further messages. Try to run `send.py` in new terminal!
