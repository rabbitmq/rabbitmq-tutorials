# Go code for RabbitMQ tutorials

Here you can find Go code examples from [RabbitMQ tutorials](https://www.rabbitmq.com/getstarted.html).

To successfully use the examples you will need a running RabbitMQ server with the [stream plugin enabled](https://www.rabbitmq.com/docs/stream#enabling-plugin).

See [First Application With RabbitMQ Streams](https://www.rabbitmq.com/blog/2021/07/19/rabbitmq-streams-first-application), [Stream plugin documentation](https://www.rabbitmq.com/docs/stream) and [how to preconfigure plugins](https://www.rabbitmq.com/docs/plugins#enabled-plugins-file).

## Requirements

These examples use the [`rabbitmq/rabbitmq-stream-go-client`](https://github.com/rabbitmq/rabbitmq-stream-go-client) client library.
Get it first with

     go get -u github.com/rabbitmq/rabbitmq-stream-go-client

## Code

Code examples are executed via `go run`:

[Tutorial one: "Hello World!"](https://www.rabbitmq.com/tutorials/tutorial-one-go-stream.html):

    go run send.go
    go run receive.go


To learn more, see [`rabbitmq/rabbitmq-stream-go-client`](https://github.com/rabbitmq/rabbitmq-stream-go-client).
