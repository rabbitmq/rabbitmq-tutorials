# Go code for RabbitMQ tutorials


Here you can find Go code examples from [RabbitMQ tutorials](https://www.rabbitmq.com/getstarted.html).


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
