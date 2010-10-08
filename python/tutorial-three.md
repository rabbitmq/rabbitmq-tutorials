

Learning RabbitMQ, part 3 (Broadcast)
=====================================


<center><div class="dot_bitmap">
<img src="http://github.com/rabbitmq/rabbitmq-tutorials/raw/master/_img/ef3c2a34713ecb41b5cc2623b0c2fc34.png" alt="Dot graph" width="491" height="125" />
</div></center>


In [previous part](http://github.com/rabbitmq/rabbitmq-tutorials/blob/master/python/tutorial-two.md) of this tutorial we created
a task queue. The core assumption behind a task queue is that a task
is delivered to exactly one worker. In this part we'll do something completely
different - we'll try to deliver a message to multiple consumers. This
pattern is known as "publish-subscribe".

To illustrate this, we're going to build a simple
logging system. It will consist of two programs - the first will emit log
messages and the second will receive and print them.

In our logging system every running copy of the receiver program
will get the same messages. That way we'll be able to run one
receiver and direct the logs to disk; and at the same time we'll be able to run
another receiver and see the same logs on the screen.

Essentially, emitted log messages are going to be broadcasted to all
the receivers.


Exchanges
---------

In previous parts of the tutorial we've understood how to send and
receive messages to and from a queue. Now it's time to introduce
the full messaging model in Rabbit.

Let's quickly cover what we've learned:

 * A _producer_ is user application that sends messages.
 * A _queue_ is a buffer that stores messages.
 * A _consumer_ is user application that receives messages.


The core idea in the messaging model in Rabbit is that the
producer never sends any messages directly to the queue. Actually,
quite often the producer doesn't even know if a message will be
delivered to any queue at all!

Instead, the producer can only send messages to an _exchange_. An
exchange is a very simple thing. On one side it receives messages from
producers and the other side it pushes them to queues. The exchange
must know exactly what to do with a received message. Should it be
appended to a particular queue? Should it be appended to many queues?
Or should it get discarded. The exact rules for that are
defined by the _exchange type_.


<center><div class="dot_bitmap">
<img src="http://github.com/rabbitmq/rabbitmq-tutorials/raw/master/_img/d8e72dacc8896f457fc9abb0d701ce84.png" alt="Dot graph" width="376" height="125" />
</div></center>



There are a few exchange types available: `direct`, `topic`,
`headers` and `fanout`. We'll focus on the last one - the
fanout. Let's create an exchange of that type, and call it `logs`:

<div><pre><code class='python'>channel.exchange_declare(exchange='logs',
                         type='fanout')</code></pre></div>


The fanout exchange is very simple. As you can probably guess from the
name, it just broadcasts all the messages it receives to all the
queues it knows. And that's exactly what we need for our logger.


> #### Listing exchanges
>
> To list the exchanges on the server you can once again use the
> Swiss Army Knife - `rabbitmqctl`:
>
>     $ sudo rabbitmqctl list_exchanges
>     Listing exchanges ...
>     logs      fanout
>     amq.direct      direct
>     amq.topic       topic
>     amq.fanout      fanout
>     amq.headers     headers
>     ...done.
>
> You can see a few `amq.` exchanges. They're created by default, but
> chances are you'll never need to use them.

<div></div>

> #### Nameless exchange
>
> In previous parts of the tutorial we knew nothing about exchanges,
> but still were able to send messages to queues. That was possible
> because we were using a default `""` _empty string_ (nameless) exchange.
> Remember how publishing worked:
>
>     channel.basic_publish(exchange='',
>                           routing_key='test',
>                           body=message)
>
> The _empty string_ exchange is special: messages are
> routed to the queue with name specified by `routing_key`.



Temporary queues
----------------

As you may remember previously we were using queues which had a specified name
(remember `test` and `task_queue`?). Being able to name a
queue was crucial for us - we needed to point the workers to the same
queue.  Essentially, giving a queue a name is important when you
want to share the queue between multiple consumers.

But that's not true for our logger. We do want to hear about all
currently flowing log messages, we do not want to receive only a subset
of messages. We're also interested only in currently flowing messages
not in the old ones. To solve that we need two things.

First, whenever we connect to Rabbit we need a fresh, empty queue. To do it
we could create a queue with a random name, or, even better - let server
choose a random queue name for us. We can do it by not supplying the
`queue` parameter to `queue_declare`:

<div><pre><code class='python'>result = channel.queue_declare()</code></pre></div>


At that point `result.queue` contains a random queue name. For example it
may look like `amq.gen-U0srCoW8TsaXjNh73pnVAw==`.

Secondly, once we disconnect the client the queue should be
deleted. There's an `auto_delete` flag for that:

<div><pre><code class='python'>result = channel.queue_declare(auto_delete=True)</code></pre></div>



Bindings
--------


<center><div class="dot_bitmap">
<img src="http://github.com/rabbitmq/rabbitmq-tutorials/raw/master/_img/45955c6ea99bae76fcb360a284cbb53b.png" alt="Dot graph" width="339" height="96" />
</div></center>



We've already created a fanout exchange and a queue. Now we need to
tell the exchange to send messages to our queue. That relationship
between exchange and a queue is called a _binding_.

<div><pre><code class='python'>channel.queue_bind(exchange='logs',
                   queue=result.queue)</code></pre></div>


From now on the `logs` exchange will broadcast all the messages also to
our queue.

> #### Listing bindings
>
> You can list existing bindings using, you guessed it,
> `rabbitmqctl list_bindings`.


Putting it all together
-----------------------


<center><div class="dot_bitmap">
<img src="http://github.com/rabbitmq/rabbitmq-tutorials/raw/master/_img/b78d29ae746c8faafb655eb616173f68.png" alt="Dot graph" width="347" height="173" />
</div></center>


The producer program, which emits log messages, doesn't look much
different than in previous tutorial. The most important change is
that, we now need to publish messages to the `logs` exchange instead of
the nameless one. We need to supply a `routing_key`, but
its value is ignored for `fanout` exchanges. Here goes the code for
`emit_log.py` script:

<div><pre><code class='python'>#!/usr/bin/env python
import pika
import sys

connection = pika.AsyncoreConnection(pika.ConnectionParameters(
        host='127.0.0.1',
        credentials=pika.PlainCredentials('guest', 'guest')))
channel = connection.channel()

message = ' '.join(sys.argv[1:]) or &quot;info: Hello World!&quot;
channel.basic_publish(exchange='logs',
                      routing_key='',
                      body=message)
print &quot; [x] Sent %r&quot; % (message,)</code></pre></div>

[(emit_log.py source)](http://github.com/rabbitmq/rabbitmq-tutorials/blob/master/python/emit_log.py)

As you see, we avoided declaring exchange here. If the `logs` exchange
isn't created at the time this code is executed the message will be
lost. That's okay for us - if no consumer is listening yet (ie:
the exchange hasn't been created) we can discard the message.

The code for `receive_logs.py`:

<div><pre><code class='python'>#!/usr/bin/env python
import pika

connection = pika.AsyncoreConnection(pika.ConnectionParameters(
        host='127.0.0.1',
        credentials=pika.PlainCredentials('guest', 'guest')))
channel = connection.channel()

channel.exchange_declare(exchange='logs',
                         type='fanout')

result = channel.queue_declare(auto_delete=True)
queue_name = result.queue

channel.queue_bind(exchange='logs',
                   queue=queue_name)

print ' [*] Waiting for logs. To exit press CTRL+C'

def callback(ch, method, header, body):
    print &quot; [x] %r&quot; % (body,)

channel.basic_consume(callback,
                      queue=queue_name,
                      no_ack=True)

pika.asyncore_loop()</code></pre></div>

[(receive_logs.py source)](http://github.com/rabbitmq/rabbitmq-tutorials/blob/master/python/receive_logs.py)


We're done. If you want to save logs to a file, just open a console and type:

    $ ./receive_logs.py > logs_from_rabbit.log

If you wish to see the logs on your screen, spawn a new terminal and run:

    $ ./receive_logs.py

And of course, to emit logs type:

    $ ./emit_log.py


