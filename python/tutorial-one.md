<!---
title: Learning RabbitMQ, part 1 (Hello world!)
layout: post
--->

Learning RabbitMQ, part 1 ("Hello world!")
==========================================

<!--
{% dot -Gsize="10,1.3" -Grankdir=LR %}
digraph G {
    P1 [label="P", {{ site.dotstyle.producer }}];
    Q1 [label="{<s>||||<e>}", {{ site.dotstyle.queue }}];
    C1 [label="C", {{ site.dotstyle.consumer }}];

    P1 -> Q1 -> C1;
}
{% enddot %}
-->


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


<!--
<div class="info" markdown="1">
-->

#### Where to get help

If you’re having trouble going through this tutorial you can post a message to
[rabbitmq-discuss mailing list](https://lists.rabbitmq.com/cgi-bin/mailman/listinfo/rabbitmq-discuss).

<!--
</div>
-->

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
<!--
    {% dot -Gsize="10,0.3" -Grankdir=LR%}
   digraph G {
       P1 [label="P", {{ site.dotstyle.producer }}];
   }
   {% enddot %}
-->
 * _A queue_ is the name for a mailbox. It lives inside RabbitMQ.
    {% dot -Gsize="10,0.3" -Grankdir=LR%}
<!--
   digraph G {
       Q1 [label="{<s>||||<e>}", {{ site.dotstyle.queue }}];
   }
   {% enddot %}
-->
 * _Consuming_ has a simmilar meaning to receiving. _Consumer_ is a program
   that mostly waits to receive messages.
<!--
    {% dot -Gsize="10,0.3" -Grankdir=LR %}
   digraph G {
       C1 [label="C", {{ site.dotstyle.consumer }}];
   }
   {% enddot %}
-->

Hello World!
------------

Our first "Hello world" won't be too complex - let's send a message, receive
it and print it on the screen. To do so we need two programs, well, one that
sends a message and one that receives and prints it.

<!--
Our overall design will look like:
{% dot -Gsize="10,0.8" -Grankdir=LR %}
digraph G {
    P1 [label="P", {{ site.dotstyle.producer }}];
    Q1 [label="{<s>||||<e>}", {{ site.dotstyle.queue }}];
    C1 [label="C", {{ site.dotstyle.consumer }}];

    P1 -> Q1 -> C1;
}
{% enddot %}
-->


Creating a program
------------------

<!--
<div class="info" markdown="1">
-->
#### RabbitMQ libraries

RabbitMQ speaks AMQP protocol. To use Rabbit you'll need a library that
understands the same protocol as Rabbit. There is a choice of libraries
for almost every programming language. Python it's not different and there is
a bunch of libraries to choose from:
 * [py-amqplib](http://barryp.org/software/py-amqplib/)
 * [txAMQP](https://launchpad.net/txamqp)
 * [pika](http://github.com/tonyg/pika)

In this tutorial we're going to use `pika`. To install it you can use [`pip`](http://pip.openplans.org/) package management tool:

    $ sudo pip install -e git://github.com/tonyg/pika.git#egg=pika

If you don't have `pip`, you may want to install it.

 * On Ubuntu:

        $ sudo apt-get install python-pip

 * On Debian:

        $ sudo apt-get install python-setuptools
        $ sudo easy_install pip

<!--
</div>
-->

### Sending


<!--
{% dot -Gsize="10,0.5" -Grankdir=LR %}
digraph G {
    P1 [label="P", {{ site.dotstyle.producer }}];
    Q1 [label="{<s>||||<e>}", {{ site.dotstyle.queue }}];

    P1 -> Q1;
}
{% enddot %}
-->

Our first program `send.py` will send a single message to the queue.
The first thing we need to do is to connect to RabbitMQ server.
<!--
{% highlight python linenos=true linenostart=1 %}
-->

    #!/usr/bin/env python
    import pika

    connection = pika.AsyncoreConnection(pika.ConnectionParameters(
                   '127.0.0.1',
                   credentials = pika.PlainCredentials('guest', 'guest'))
    channel = connection.channel()
<!--
{% endhighlight %}
-->

Whenever we send a message we need to make sure the recipient queue exists.
RabbitMQ will just trash the message if can't deliver it. So, we need to
create a queue to which the message will be delivered. Let's name this queue
_test_:
<!--
{% highlight python linenos=true linenostart=8 %}
-->

    channel.queue_declare(queue='test')
<!--
{% endhighlight %}
-->

At that point we're ready to send a message. Our first message will contain
a string _Hello World!_. We are going to send it to our _test_ queue:
<!--
{% highlight python linenos=true linenostart=9 %}
-->

    channel.basic_publish(exchange='',
                          routing_key='test',
                          body='Hello World!')
    print " [x] Sent 'Hello World!'"
<!--
{% endhighlight %}
-->

[(full send.py source)](http://github.com/rabbitmq/rabbitmq-tutorials/blob/master/python/send.py)

### Receiving

<!--
{% dot -Gsize="10,0.5" -Grankdir=LR %}
digraph G {
    rankdir=LR;

    Q1 [label="{<s>||||<e>}", {{ site.dotstyle.queue }}];
    C1 [label="C", {{ site.dotstyle.consumer }}];

    Q1 -> C1;
}
{% enddot %}
-->

Our second program `receive.py` will receive messages from the queue and print
them on the screen. 

The code responsible for connecting to Rabbit is the same as in previous example.
You can copy the first 7 lines.

     # ... connection code is the same, copy first 7 lines from send.py ...

Just like before, in the beginning we must make sure that the
queue exists. Creating a queue using `queue_declare` is idempotent - you can
run the command as many times you'd like, and only one queue will be created.
<!--
{% highlight python linenos=true linenostart=8 %}
-->

    channel.queue_declare(queue='test')
<!--
{% endhighlight %}
-->

Receiving messages from the queue is a bit more complex. Whenever we receive
a message, a `callback` function is called. In our case
this function will print on the screen the contents of the message.
<!--
{% highlight python linenos=true linenostart=9 %}
-->

    def callback(ch, method, header, body):
        print " [x] Received %.20r" % (body,)
<!--
{% endhighlight %}
-->

Next, we need to tell RabbitMQ that this particular callback function is
interested in messages from our _test_ queue:
<!--
{% highlight python linenos=true linenostart=11 %}
-->

    channel.basic_consume(callback,
                          queue='test',
                          no_ack=True)
<!--
{% endhighlight %}
-->

And finally, we enter never-ending loop that waits for data and runs callbacks
whenever necessary.
<!--
{% highlight python linenos=true linenostart=14 %}
-->

    print ' [*] Waiting for messages. To exit press CTRL+C'
    pika.asyncore_loop()
<!--
{% endhighlight %}
-->
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
