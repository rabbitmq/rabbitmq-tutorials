# Elixir code for RabbitMQ tutorials

Here you can find Elixir code examples from [RabbitMQ tutorials](https://www.rabbitmq.com/getstarted.html).

## Requirements

These examples use the [`rabbitmq-community/rabbitmq-stream-elixir-client`](https://github.com/rabbitmq-community/rabbitmq-stream-elixir-client) client library.

The dependencies are installed during the exection of the examples using `Mix.install/1`

## Code

Code examples are executed via `elixir`:

Tutorial one: "Hello World!":

``` shell
# run the publisher
elixir publish.exs

# run the consumer
elixir consume.exs
```

Offset tracking tutorial:

``` shell
# run the publisher
elixir offset_tracking_send.exs

# run the consumer
elixir offset_tracking_receive.exs
```

To learn more, see [`rabbitmq-community/rabbitmq-stream-elixir-client`](https://github.com/rabbitmq-community/rabbitmq-stream-elixir-client).
