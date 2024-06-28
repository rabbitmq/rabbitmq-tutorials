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

    rebar3 send
    rebar3 recv

[Tutorial two: Work Queues](https://www.rabbitmq.com/tutorials/tutorial-two-python.html):

    rebar3 new_task
    rebar3 worker

[Tutorial three: Publish/Subscribe](https://www.rabbitmq.com/tutorials/tutorial-three-python.html):

    rebar3 receive_logs
    rebar3 emit_log

[Tutorial four: Routing](https://www.rabbitmq.com/tutorials/tutorial-four-python.html):

    rebar3 receive_logs_direct
    rebar3 emit_log_direct

[Tutorial five: Topics](https://www.rabbitmq.com/tutorials/tutorial-five-python.html):

    rebar3 receive_logs_topic
    rebar3 emit_log_topic

[Tutorial Six: RPC](https://www.rabbitmq.com/tutorials/tutorial-six-python.html):

    rebar3 rpc_server
    rebar3 rpc_client
