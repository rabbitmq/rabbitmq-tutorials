

Learning RabbitMQ, part 2 (Task Queue)
======================================



<center><div class="dot_bitmap">
<img src="http://github.com/rabbitmq/rabbitmq-tutorials/raw/master/_img/378bf43b3d9dcc790d2113257412c928.png" alt="Dot graph" width="376" height="125" />
</div></center>



In the first part of this tutorial we've learned how to send messages
to and receive from a named queue. In this part we'll try to create a
_Task Queue_ to distribute time-consuming work across multiple
workers.

The main idea behind Task Queues (aka: _Work Queues_) is to avoid
doing resource intensive tasks immediately. Instead, we encapsulate
the task in a message and put it on to the queue. A worker process
will pop the task and eventually execute the job.

This concept is especially useful in web applications where it's
impossible to handle a complex task during a short http request
window.


Preparations
------------

In previous part of this tutorial we were sending a message containing
"Hello World!" string. In this part we'll be sending strings that
stand for complex tasks. As we don't have any real hard tasks, like
image to be resized or pdf files to be rendered, let's fake it by just
pretending we're busy - by using `time.sleep()` function. We'll take
the number of dots in the string as a complexity, every dot will
account for one second of "work".  For example, a fake task described
by `Hello!...` will take three seconds.

We need to slightly modify our `send.py` code, to allow sending
arbitrary messages from command line:

<table class="highlighttable"><tr><td class="linenos"><div class="linenodiv"><pre><code class="python">12
13
14
15
16</code></pre></div></td><td class="code"><div class="highlight"><pre><span class="kn">import</span> <span class="nn">sys</span>
<span class="n">message</span> <span class="o">=</span> <span class="s">&#39; &#39;</span><span class="o">.</span><span class="n">join</span><span class="p">(</span><span class="n">sys</span><span class="o">.</span><span class="n">argv</span><span class="p">[</span><span class="mi">1</span><span class="p">:])</span> <span class="ow">or</span> <span class="s">&quot;Hello World!&quot;</span>
<span class="n">channel</span><span class="o">.</span><span class="n">basic_publish</span><span class="p">(</span><span class="n">exchange</span><span class="o">=</span><span class="s">&#39;&#39;</span><span class="p">,</span> <span class="n">routing_key</span><span class="o">=</span><span class="s">&#39;test&#39;</span><span class="p">,</span>
                      <span class="n">body</span><span class="o">=</span><span class="n">message</span><span class="p">)</span>
<span class="k">print</span> <span class="s">&quot; [x] Sent </span><span class="si">%r</span><span class="s">&quot;</span> <span class="o">%</span> <span class="p">(</span><span class="n">message</span><span class="p">,)</span>
</pre></div>
</td></tr></table>


Our `receive.py` script also requires some changes: it needs to fake a
second of work for every dot in the message body:

<table class="highlighttable"><tr><td class="linenos"><div class="linenodiv"><pre><code class="python">12
13
14
15
16
17</code></pre></div></td><td class="code"><div class="highlight"><pre><span class="kn">import</span> <span class="nn">time</span>

<span class="k">def</span> <span class="nf">callback</span><span class="p">(</span><span class="n">ch</span><span class="p">,</span> <span class="n">method</span><span class="p">,</span> <span class="n">header</span><span class="p">,</span> <span class="n">body</span><span class="p">):</span>
    <span class="k">print</span> <span class="s">&quot; [x] Received </span><span class="si">%r</span><span class="s">&quot;</span> <span class="o">%</span> <span class="p">(</span><span class="n">body</span><span class="p">,)</span>
	<span class="n">time</span><span class="o">.</span><span class="n">sleep</span><span class="p">(</span> <span class="n">body</span><span class="o">.</span><span class="n">count</span><span class="p">(</span><span class="s">&#39;.&#39;</span><span class="p">)</span> <span class="p">)</span>
    <span class="k">print</span> <span class="s">&quot; [x] Done&quot;</span>
</pre></div>
</td></tr></table>


Round-robin dispatching
-----------------------

The main advantage of pushing fat tasks through the Task Queue is the
ability to easily parallelize work. If we have too much work for us to
handle, we can just add more workers and scale easily.

First, let's try to run two `worker.py` scripts in the same time. They
will both try to get messages from the queue, but how exactly? Let's
see.

You need three consoles open. First two to run `worker.py`
script. These consoles will be our two consumers - C1 and C2. On the
third one we'll be publishing new tasks. Once you've started the
consumers you can produce few messages:

    shell3$ ./new_task.py First message.
    shell3$ ./new_task.py Second message..
    shell3$ ./new_task.py Third message...
    shell3$ ./new_task.py Fourth message....
    shell3$ ./new_task.py Fifth message.....

And let's see what is delivered to our workers:

    shell1$ ./worker.py
     [*] Waiting for messages. To exit press CTRL+C
     [x] Received 'First message.'
     [x] Received 'Third message...'
     [x] Received 'Fifth message.....'

    shell2$ ./worker.py
     [*] Waiting for messages. To exit press CTRL+C
     [x] Received 'Second message..'
     [x] Received 'Fourth message....'

By default, RabbitMQ will send every n-th message to a consumer. That
way on average every consumer will get the same number of
messages. This way of distributing messages is called round-robin. Try
this out with three or more workers.

Message acknowledgments
-----------------------

Doing our tasks can take a few seconds. You may wonder what happens if
one of the consumers got hard job and has died while doing it. With
our current code once RabbitMQ delivers message to the customer it
immediately removes it from memory. In our case if you kill a worker
we will loose the message it was just processing. We'll also loose all
the messages that were dispatched to this particular worker and not
yet handled.

We don't want to loose any task. If a workers dies, we'd like the task
to be delivered to another worker.

In order to make sure a message is never lost by the worker, RabbitMQ
supports message _acknowledgments_. It's basically an information,
sent back from the consumer which tell Rabbit that particular message
had been received, fully processed and that Rabbit is free to delete
it.

If consumer dies without sending ack, Rabbit will understand that a
message wasn't processed fully and will dispatch it to another
consumer. That way you can be sure that no message is lost, even if
the workers occasionaly die.

But there aren't any message timeouts, Rabbit will redispatch the
message again only when the worker connection dies.

Acknowledgments are turned on by default. Though, in previous
examples we had explicitly turned them off: `no_ack=True`. It's time
to remove this flag and send a proper acknowledgment from the worker,
once we're done with a task.

<table class="highlighttable"><tr><td class="linenos"><div class="linenodiv"><pre><code class="python">12
13
14
15
16
17
18
19</code></pre></div></td><td class="code"><div class="highlight"><pre><span class="k">def</span> <span class="nf">callback</span><span class="p">(</span><span class="n">ch</span><span class="p">,</span> <span class="n">method</span><span class="p">,</span> <span class="n">header</span><span class="p">,</span> <span class="n">body</span><span class="p">):</span>
    <span class="k">print</span> <span class="s">&quot; [x] Received </span><span class="si">%r</span><span class="s">&quot;</span> <span class="o">%</span> <span class="p">(</span><span class="n">body</span><span class="p">,)</span>
	<span class="n">time</span><span class="o">.</span><span class="n">sleep</span><span class="p">(</span> <span class="n">body</span><span class="o">.</span><span class="n">count</span><span class="p">(</span><span class="s">&#39;.&#39;</span><span class="p">)</span> <span class="p">)</span>
    <span class="k">print</span> <span class="s">&quot; [x] Done&quot;</span>
    <span class="n">ch</span><span class="o">.</span><span class="n">basic_ack</span><span class="p">(</span><span class="n">delivery_tag</span> <span class="o">=</span> <span class="n">method</span><span class="o">.</span><span class="n">delivery_tag</span><span class="p">)</span>

<span class="n">channel</span><span class="o">.</span><span class="n">basic_consume</span><span class="p">(</span><span class="n">callback</span><span class="p">,</span>
                      <span class="n">queue</span><span class="o">=</span><span class="s">&#39;test&#39;</span><span class="p">)</span>
</pre></div>
</td></tr></table>

Using that code we may be sure that even if you kill a worker using
CTRL+C while it was processing a message, it will won't be lost.  Soon
after the worker dies all unacknowledged messages will be redispatched.


Message durability
------------------

We learned what to do to make sure that even if the consumer dies, the
task isn't lost. But our tasks will still be lost if RabbitMQ server
dies.

When RabbitMQ quits or crashes it will forget the queues and messages
unless you tell it not to.

First, we need to make sure that Rabbit will never loose our `test`
queue. In order to do so, we need to declare it as _durable_:

    channel.queue_declare(queue='test', durable=True)

Although that command is correct by itself it won't work in our
setup. That's because we've already defined a queue called `test`
which is not durable. RabbitMQ doesn't allow you to redefine a queue
with different parameters and will return hard error to any program
that tries to do that. But there is a quick workaround - let's just
declare a queue with different name, for example `test_dur`:

    channel.queue_declare(queue='test_dur', durable=True)

This `queue_declare` change needs to be applied to both the producer
and consumer code.

At that point we're sure that the `test_dur` queue won't be lost even
if RabbitMQ dies. Now we need to make our messages persistent - by
suppling a `delivery_mode` header with a value `2`:

    channel.basic_publish(exchange='', routing_key="test_dur",
                          body=message,
                          properties=pika.BasicProperties(
                             delivery_mode = 2, # make message persistent
                          ))

Marking messages as persistent doesn't really guarantee that a message
will survive. Although it tells Rabbit to save message to the disk,
there is still a time window when Rabbit accepted a message and
haven't it yet saved. Also, Rabbit doesn't do `fsync(2)` for every
message - it may be just saved to caches and not really written to the
disk. The persistence guarantees are weak, but it's more than enough
for our task queue. If you need stronger guarantees you can wrap the
publishing code in a _transaction_.


Fair dispatching
----------------

You might have noticed that the dispatching still doesn't work exactly
as we want to. For example in a situation with two workers, when all
odd messages are heavy and even messages are light, one worker will be
constantly busy and the other one will do hardly any work. Well,
Rabbit doesn't know anything about that and will still dispatch
messages evenly.

That happens because Rabbit dispatches message just when a message
enters the queue. It doesn't look in number of unacknowledged messages
for a consumer. It just blindly dispatches every n-th message to a
every consumer.


<center><div class="dot_bitmap">
<img src="http://github.com/rabbitmq/rabbitmq-tutorials/raw/master/_img/e7e7b4568fed280746fc83b370cd60ea.png" alt="Dot graph" width="414" height="125" />
</div></center>


In order to defeat that we may use `basic.qos` method with the
`prefetch_count` settings. That allows us to tell Rabbit not to give
more than one message to a worker at a time. Or, in other words, don't
dispatch a new message to a worker until it has processed and
acknowledged previous one.

<table class="highlighttable"><tr><td class="linenos"><div class="linenodiv"><pre><code class="python">17</code></pre></div></td><td class="code"><div class="highlight"><pre><span class="n">channel</span><span class="o">.</span><span class="n">basic_qos</span><span class="p">(</span><span class="n">prefetch_count</span><span class="o">=</span><span class="mi">1</span><span class="p">)</span>
</pre></div>
</td></tr></table>


Putting it all together
-----------------------

Final code of our `new_task.py` script:

<table class="highlighttable"><tr><td class="linenos"><div class="linenodiv"><pre><code class="python"> 1
 2
 3
 4
 5
 6
 7
 8
 9
10
11
12
13
14
15
16
17
18</code></pre></div></td><td class="code"><div class="highlight"><pre><span class="c">#!/usr/bin/env python</span>
<span class="kn">import</span> <span class="nn">pika</span>
<span class="kn">import</span> <span class="nn">sys</span>

<span class="n">connection</span> <span class="o">=</span> <span class="n">pika</span><span class="o">.</span><span class="n">AsyncoreConnection</span><span class="p">(</span><span class="n">pika</span><span class="o">.</span><span class="n">ConnectionParameters</span><span class="p">(</span>
        <span class="n">host</span><span class="o">=</span><span class="s">&#39;127.0.0.1&#39;</span><span class="p">,</span>
        <span class="n">credentials</span><span class="o">=</span><span class="n">pika</span><span class="o">.</span><span class="n">PlainCredentials</span><span class="p">(</span><span class="s">&#39;guest&#39;</span><span class="p">,</span> <span class="s">&#39;guest&#39;</span><span class="p">)))</span>
<span class="n">channel</span> <span class="o">=</span> <span class="n">connection</span><span class="o">.</span><span class="n">channel</span><span class="p">()</span>

<span class="n">channel</span><span class="o">.</span><span class="n">queue_declare</span><span class="p">(</span><span class="n">queue</span><span class="o">=</span><span class="s">&#39;test_dur&#39;</span><span class="p">,</span> <span class="n">durable</span><span class="o">=</span><span class="bp">True</span><span class="p">)</span>

<span class="n">message</span> <span class="o">=</span> <span class="s">&#39; &#39;</span><span class="o">.</span><span class="n">join</span><span class="p">(</span><span class="n">sys</span><span class="o">.</span><span class="n">argv</span><span class="p">[</span><span class="mi">1</span><span class="p">:])</span> <span class="ow">or</span> <span class="s">&quot;Hello World!&quot;</span>
<span class="n">channel</span><span class="o">.</span><span class="n">basic_publish</span><span class="p">(</span><span class="n">exchange</span><span class="o">=</span><span class="s">&#39;&#39;</span><span class="p">,</span> <span class="n">routing_key</span><span class="o">=</span><span class="s">&#39;test&#39;</span><span class="p">,</span>
                      <span class="n">body</span><span class="o">=</span><span class="n">message</span>
                      <span class="n">properties</span><span class="o">=</span><span class="n">pika</span><span class="o">.</span><span class="n">BasicProperties</span><span class="p">(</span>
                         <span class="n">delivery_mode</span> <span class="o">=</span> <span class="mi">2</span><span class="p">,</span> <span class="c"># make message persistent</span>
                      <span class="p">))</span>
<span class="k">print</span> <span class="s">&quot; [x] Sent </span><span class="si">%r</span><span class="s">&quot;</span> <span class="o">%</span> <span class="p">(</span><span class="n">message</span><span class="p">,)</span>
</pre></div>
</td></tr></table>

And our worker:

<table class="highlighttable"><tr><td class="linenos"><div class="linenodiv"><pre><code class="python"> 1
 2
 3
 4
 5
 6
 7
 8
 9
10
11
12
13
14
15
16
17
18
19
20
21
22
23</code></pre></div></td><td class="code"><div class="highlight"><pre><span class="c">#!/usr/bin/env python</span>
<span class="kn">import</span> <span class="nn">pika</span>
<span class="kn">import</span> <span class="nn">time</span>

<span class="n">connection</span> <span class="o">=</span> <span class="n">pika</span><span class="o">.</span><span class="n">AsyncoreConnection</span><span class="p">(</span><span class="n">pika</span><span class="o">.</span><span class="n">ConnectionParameters</span><span class="p">(</span>
        <span class="n">host</span><span class="o">=</span><span class="s">&#39;127.0.0.1&#39;</span><span class="p">,</span>
        <span class="n">credentials</span><span class="o">=</span><span class="n">pika</span><span class="o">.</span><span class="n">PlainCredentials</span><span class="p">(</span><span class="s">&#39;guest&#39;</span><span class="p">,</span> <span class="s">&#39;guest&#39;</span><span class="p">)))</span>
<span class="n">channel</span> <span class="o">=</span> <span class="n">connection</span><span class="o">.</span><span class="n">channel</span><span class="p">()</span>

<span class="n">channel</span><span class="o">.</span><span class="n">queue_declare</span><span class="p">(</span><span class="n">queue</span><span class="o">=</span><span class="s">&#39;test&#39;</span><span class="p">)</span>
<span class="k">print</span> <span class="s">&#39; [*] Waiting for messages. To exit press CTRL+C&#39;</span>

<span class="k">def</span> <span class="nf">callback</span><span class="p">(</span><span class="n">ch</span><span class="p">,</span> <span class="n">method</span><span class="p">,</span> <span class="n">header</span><span class="p">,</span> <span class="n">body</span><span class="p">):</span>
    <span class="k">print</span> <span class="s">&quot; [x] Received </span><span class="si">%r</span><span class="s">&quot;</span> <span class="o">%</span> <span class="p">(</span><span class="n">body</span><span class="p">,)</span>
    <span class="n">time</span><span class="o">.</span><span class="n">sleep</span><span class="p">(</span> <span class="n">body</span><span class="o">.</span><span class="n">count</span><span class="p">(</span><span class="s">&#39;.&#39;</span><span class="p">)</span> <span class="p">)</span>
    <span class="k">print</span> <span class="s">&quot; [x] Done&quot;</span>
    <span class="n">ch</span><span class="o">.</span><span class="n">basic_ack</span><span class="p">(</span><span class="n">delivery_tag</span> <span class="o">=</span> <span class="n">method</span><span class="o">.</span><span class="n">delivery_tag</span><span class="p">)</span>

<span class="n">channel</span><span class="o">.</span><span class="n">basic_qos</span><span class="p">(</span><span class="n">prefetch_count</span><span class="o">=</span><span class="mi">1</span><span class="p">)</span>
<span class="n">channel</span><span class="o">.</span><span class="n">basic_consume</span><span class="p">(</span><span class="n">callback</span><span class="p">,</span>
                      <span class="n">queue</span><span class="o">=</span><span class="s">&#39;test&#39;</span><span class="p">)</span>

<span class="n">pika</span><span class="o">.</span><span class="n">asyncore_loop</span><span class="p">()</span>
</pre></div>
</td></tr></table>


Using message acknowledgments and `prefetch_count` you may set up
quite a decent work queue. The durabiltiy options will let the tasks
to survive even if Rabbit is killed.

Now we can move on to part 3 of this tutorial and learn how to
distribute the same message to many consumers.
