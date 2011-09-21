# Erlang code for RabbitMQ tutorials #

Here you can find a Erlang code examples from [RabbitMQ
tutorials](http://www.rabbitmq.com/getstarted.html).

This code is using [RabbitMQ Erlang
Client](http://hg.rabbitmq.com/rabbitmq-erlang-client/) ([User
Guide](http://www.rabbitmq.com/erlang-client-user-guide.html)).

## Requirements

To run this code you need at least [Erlang
R13B03](http://erlang.org/download.html), on Ubuntu you can get it
using apt:

    sudo apt-get install erlang

You need Erlang Client binaries:

    wget http://www.rabbitmq.com/releases/plugins/v2.5.0/rabbit_common-2.5.0.ez
    unzip rabbit_common-2.5.0.ez
    ln -s rabbit_common-2.5.0 rabbit_common

    wget http://www.rabbitmq.com/releases/plugins/v2.5.0/amqp_client-2.5.0.ez
    unzip amqp_client-2.5.0.ez
    ln -s amqp_client-2.5.0 amqp_client


## Code

[Tutorial one: "Hello World!"](http://www.rabbitmq.com/tutorial-one-python.html):

    ./send.erl
    ./receive.erl

[Tutorial two: Work Queues](http://www.rabbitmq.com/tutorial-two-python.html):

    ./new_task.erl "A very hard task which takes two seconds.."
    ./worker.erl

[Tutorial three: Publish/Subscribe](http://www.rabbitmq.com/tutorial-three-python.html):

    ./receive_logs.erl
    ./emit_log.erl "info: This is the log message"

[Tutorial four: Routing](http://www.rabbitmq.com/tutorial-four-python.html):

    ./receive_logs_direct.erl info
    ./emit_log_direct.erl info Hello

[Tutorial five: Topics](http://www.rabbitmq.com/tutorial-five-python.html):

    ./receive_logs_topic.erl "*.rabbit"
    ./emit_log_topic.erl red.rabbit Hello
