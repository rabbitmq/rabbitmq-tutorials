# Elixir code for RabbitMQ tutorials

Here you can find an [Elixir](http://elixir-lang.org) port of
[RabbitMQ tutorials](http://www.rabbitmq.com/getstarted.html).

## Requirements

To run this code you need a [recent Elixir version installed](http://elixir-lang.org/install.html),
which should include [Mix, the Elixir build tool](http://elixir-lang.org/docs/stable/mix/Mix.html).

These tutorials use [Elixir AMQP 0-9-1 client](https://github.com/pma/amqp) built
on top of the official [RabbitMQ Erlang client](https://www.rabbitmq.com/erlang-client-user-guide.html).

To install dependencies with Mix, run

    mix deps.get
    mix deps.compile

## Code

To run the examples, use `mix run`.

Tutorial one: "Hello World!":

    mix run receive.exs
    mix run send.exs

Tutorial two: Work Queues

    mix run worker.exs
    mix run new_task.exs

Tutorial three: Publish/Subscribe

    mix run receive_logs.exs
    mix run emit_log.exs

Tutorial four: Routing

    mix run receive_logs_direct.exs --info --warning
    mix run emit_log_direct.exs --info "A message"

Tutorial five: Topics

    mix run receive_logs_topic.exs "info.*" "warn.*"
    mix run emit_log_topic.exs "info.connections" "Connected"

Tutorial six: RPC (Request/Response)

    mix run rpc_server.exs
    mix run rpc_client.exs
