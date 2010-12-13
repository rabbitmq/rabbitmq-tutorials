# Erlang code for RabbitMQ tutorials #

Here you can find a Erlang code examples from [RabbitMQ
tutorials](http://www.rabbitmq.com/getstarted.html).

This code is using [RabbitMQ Erlang
Client](http://hg.rabbitmq.com/rabbitmq-erlang-client/) ([User
Guide](http://www.rabbitmq.com/erlang-client-user-guide.html)).


## Requirements

You need Erlang Client binaries:

    wget http://www.rabbitmq.com/releases/plugins/v2.2.0/rabbit_common-2.2.0.ez
    unzip rabbit_common-2.2.0.ez

    wget http://www.rabbitmq.com/releases/plugins/v2.2.0/amqp_client-2.2.0.ez
    unzip amqp_client-2.2.0.ez


## Code

[Tutorial one: "Hello World!"](http://www.rabbitmq.com/tutorial-one-python.html):

    ./send.erl
    ./receive.erl

[Tutorial two: Work Queues](http://www.rabbitmq.com/tutorial-two-python.html):

    ./new_task.erl
    ./worker.erl

[Tutorial three: Publish/Subscribe](http://www.rabbitmq.com/tutorial-three-python.html):

    ./emit_log.erl
    ./receive_logs.erl
