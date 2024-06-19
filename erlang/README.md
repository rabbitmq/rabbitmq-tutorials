# Erlang code for RabbitMQ tutorials #

Here you can find a Erlang code examples from [RabbitMQ
tutorials](https://www.rabbitmq.com/getstarted.html).

This code is using [RabbitMQ Erlang
Client](https://github.com/rabbitmq/rabbitmq-server/tree/main/deps/amqp_client) ([User
Guide](https://www.rabbitmq.com/erlang-client-user-guide.html)).

## Requirements

To run this code you need at least [Erlang
R13B03](https://www.erlang.org/downloads), on Ubuntu you can get it
using apt:

    sudo apt-get install erlang

You also need rebar3: https://www.rebar3.org/docs/getting-started/

You need Erlang Client binaries:

    rebar3 compile

## Code

[Tutorial one: "Hello World!"](https://www.rabbitmq.com/tutorials/tutorial-one-python.html):

    rebar3 shell --eval 'send:start(), init:stop().'
    rebar3 shell --eval 'recv:start(), init:stop().'

[Tutorial two: Work Queues](https://www.rabbitmq.com/tutorials/tutorial-two-python.html):

    rebar3 shell --eval 'new_task:start(["A very hard task which takes two seconds"]), init:stop().'
    rebar3 shell --eval 'worker:start(), init:stop().'

[Tutorial three: Publish/Subscribe](https://www.rabbitmq.com/tutorials/tutorial-three-python.html):

    rebar3 shell --eval 'receive_logs:start(), init:stop().'
    rebar3 shell --eval 'emit_log:start(["Info: This is the log message"]), init:stop().'

[Tutorial four: Routing](https://www.rabbitmq.com/tutorials/tutorial-four-python.html):

    rebar3 shell --eval 'receive_logs_direct:start(["info"]), init:stop().'
    rebar3 shell --eval 'emit_log_direct:start(["info", "Hello"]), init:stop().'

[Tutorial five: Topics](https://www.rabbitmq.com/tutorials/tutorial-five-python.html):

    rebar3 shell --eval 'receive_logs_topic:start(["*.rabbit"]), init:stop().'
    rebar3 shell --eval 'emit_log_topic:start(["red.rabbit", "Hello"]), init:stop().'
