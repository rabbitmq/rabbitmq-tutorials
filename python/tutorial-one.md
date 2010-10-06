

Learning RabbitMQ, part 1 ("Hello world!")
==========================================



<center><div class="dot_bitmap">
<img src="http://github.com/rabbitmq/rabbitmq-tutorials/raw/master/_img/f102830d0b42138245d6a95010dad710.png" alt="Dot graph" width="392" height="59" />
</div></center>




Throughout this tutorial, we'll teach you the basic concepts required for
creating RabbitMQ applications. The tutorial will be illustrated with
code snippets written in [Python](http://www.python.org/). But don't worry if
you don't know this language - the core ideas are the same for other languages.

This tutorial consists of three parts:

 * First we're going to write the simplest possible "Hello World" example.
 * Next we'll try to use Rabbit as a simple "Work queue" server.
 * Finally, we'll discuss the "Publish-subscribe" pattern.

You need to have RabbitMQ server installed to go through this tutorial.
If you haven't installed it yet you can follow the
[installation instructions](http://www.rabbitmq.com/install.html).
You can tell RabbitMQ is installed by running:

    $ sudo rabbitmqctl status

If you have installed RabbitMQ you should see something like:

    Status of node rabbit@example ...
    [{running_applications,[{os_mon,"CPO  CXC 138 46","2.2.5"},
                            {sasl,"SASL  CXC 138 11","2.1.9"},
                            {mnesia,"MNESIA  CXC 138 12","4.4.13"},
                            {stdlib,"ERTS  CXC 138 10","1.16.5"},
                            {kernel,"ERTS  CXC 138 10","2.13.5"}]},
     {nodes,[{disc,[rabbit@example]}]},
     {running_nodes,[rabbit@example]}]
    ...done.


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
post office and a postman.

The major difference between RabbitMQ and the post office is the fact that it
doesn't deal with the paper, instead it accepts, stores and forwards binary
blobs of data - _messages_.

RabbitMQ uses a weird jargon, but it's simple once you'll get it. For example:

 * _Producing_ means nothing more than sending. A program that sends messages
   is a _producer_.

    
<center><div class="dot_bitmap">
<img src="http://github.com/rabbitmq/rabbitmq-tutorials/raw/master/_img/5a5fa54ed009ea0185a5f0131d1faff6.png" alt="Dot graph" width="41" height="29" />
</div></center>


 * _A queue_ is the name for a mailbox. It lives inside
   RabbitMQ. Although messages flow through RabbitMQ and your
   applications, they can be stored only inside a _queue_. A _queue_
   is not bound by any limits, it can store how many messages you
   like - it's essentially an infinite buffer. Many _producers_ can send
   messages that go to the one queue, many _consumers_ can try to
   receive data from one _queue_.

    
<center><div class="dot_bitmap">
<img src="http://github.com/rabbitmq/rabbitmq-tutorials/raw/master/_img/9bdb70c65ab8b2aa1f6b0b85c2931a54.png" alt="Dot graph" width="76" height="29" />
</div></center>


 * _Consuming_ has a simmilar meaning to receiving. _Consumer_ is a program
   that mostly waits to receive messages.

    
<center><div class="dot_bitmap">
<img src="http://github.com/rabbitmq/rabbitmq-tutorials/raw/master/_img/c81197642d0b8ac05c7e7e89b65f2806.png" alt="Dot graph" width="41" height="29" />
</div></center>



Hello World!
------------

Our "Hello world" won't be too complex - let's send a message, receive
it and print it on the screen. To do so we need two programs: one that
sends a message and one that receives and prints it.


Our overall design will look like:

<center><div class="dot_bitmap">
<img src="http://github.com/rabbitmq/rabbitmq-tutorials/raw/master/_img/b3f3e23007aca653c04def0f8e859d18.png" alt="Dot graph" width="363" height="125" />
</div></center>


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
> In this tutorial we're going to use `pika`. To install it you can use
> [`pip`](http://pip.openplans.org/) package management tool:
>
>    $ sudo pip install -e git+http://github.com/tonyg/pika.git#egg=pika
>
>If you don't have `pip`, you may want to install it.
>
> * On Ubuntu:
>
>       $ sudo apt-get install python-pip
>
> * On Debian:
>
>       $ sudo apt-get install python-setuptools
>       $ sudo easy_install pip

### Sending


<center><div class="dot_bitmap">
<img src="http://github.com/rabbitmq/rabbitmq-tutorials/raw/master/_img/090975cc54ab88a30c0bb4d47611b674.png" alt="Dot graph" width="278" height="125" />
</div></center>



Our first program `send.py` will send a single message to the queue.
The first thing we need to do is connect to RabbitMQ server.

<div>
  <pre>
    <code class='python'>#!/usr/bin/env python
import pika

connection = pika.AsyncoreConnection(pika.ConnectionParameters(
               '127.0.0.1',
               credentials = pika.PlainCredentials('guest', 'guest'))
channel = connection.channel()</code>
  </pre>
</div>



Whenever we send a message we need to make sure the recipient queue exists.
RabbitMQ will just trash the message if can't deliver it. So, we need to
create a queue to which the message will be delivered. Let's name this queue
_test_:

<div>
  <pre>
    <code class='python'>channel.queue_declare(queue='test')</code>
  </pre>
</div>



At that point we're ready to send a message. Our first message will
contain a string _Hello World!_ and we want to send it to our _test_
queue.

In RabbitMQ a message never goes directly to the queue, it always
needs to go through an _exchange_. But let's not get dragged by the
details - you can read more about _exchanges_ in third part of this
tutorial. All we need to know now is how to use a default exchange
identified by an empty string. That exchange is a special one that
allows us to specify exactly to which queue the message should go.
The queue name is specified by the `routing_key` variable:

<div>
  <pre>
    <code class='python'>channel.basic_publish(exchange='',
                      routing_key='test',
                      body='Hello World!')
print &quot; [x] Sent 'Hello World!'&quot;</code>
  </pre>
</div>



[(full send.py source)](http://github.com/rabbitmq/rabbitmq-tutorials/blob/master/python/send.py)

### Receiving



<center><div class="dot_bitmap">
<img src="http://github.com/rabbitmq/rabbitmq-tutorials/raw/master/_img/e8c961b5097209b7e18281754f6403a4.png" alt="Dot graph" width="278" height="125" />
</div></center>



Our second program `receive.py` will receive messages from the queue and print
them on the screen.

The code responsible for connecting to Rabbit is the same as the previous example.
You can copy the first 7 lines.

    # ... connection code is the same, copy first 7 lines from send.py ...

Just like before, in the beginning we must make sure that the
queue exists. Creating a queue using `queue_declare` is idempotent - you can
run the command as many times you like, and only one queue will be created.

<div>
  <pre>
    <code class='python'>channel.queue_declare(queue='test')</code>
  </pre>
</div>


You may ask why to declare queue again - we have already declared it
in our previous code. We could have avoided that if we always run the
`send.py` program before this one. But we're not sure yet which
program to run as first. In such case it's a good practice to repeat
declaring the queue in both programs.


Receiving messages from the queue is a bit more complex. Whenever we receive
a message, a `callback` function is called. In our case
this function will print on the screen the contents of the message.

<div>
  <pre>
    <code class='python'>def callback(ch, method, header, body):
    print &quot; [x] Received %.20r&quot; % (body,)</code>
  </pre>
</div>



Next, we need to tell RabbitMQ that this particular callback function is
interested in messages from our _test_ queue:

<div>
  <pre>
    <code class='python'>channel.basic_consume(callback,
                      queue='test',
                      no_ack=True)</code>
  </pre>
</div>


For that command to succeed we must be sure that a queue which we want
to subscribe to exists. Fortunately we're confident about that - we've
created a queue above - using `queue_declare`.

And finally, we enter a never-ending loop that waits for data and runs callbacks
whenever necessary.

<div>
  <pre>
    <code class='python'>print ' [*] Waiting for messages. To exit press CTRL+C'
pika.asyncore_loop()</code>
  </pre>
</div>


[(full receive.py source)](http://github.com/rabbitmq/rabbitmq-tutorials/blob/master/python/receive.py)

### Putting it all together

Now we can try out our programs. First, let's send a message using our
`send.py` program:

    $ python send.py
     [x] Sent 'Hello World!'

Let's receive it:

    $ python receive.py
     [*] Waiting for messages. To exit press CTRL+C
     [x] Received 'Hello World!'

Hurray! We were able to send our first message through RabbitMQ. As you might
have noticed, the `receive.py` program didn't exit. It will stay ready to
receive further messages. Try to run `send.py` in a new terminal!

We've learned how to send and receive a message from a named
queue. It's time to move on to part 2 of this tutorial and build a
simple _task queue_.

