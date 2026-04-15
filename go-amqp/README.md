# Go code for RabbitMQ tutorials (AMQP 1.0)


Here you can find Go code examples from [RabbitMQ tutorials](https://www.rabbitmq.com/getstarted.html), using the AMQP 1.0 client.


## Requirements

These examples use the [`rabbitmq-amqp-go-client`](https://github.com/rabbitmq/rabbitmq-amqp-go-client) library for RabbitMQ 4.x. Get it with

    go get github.com/rabbitmq/rabbitmq-amqp-go-client

A RabbitMQ node must be running on `localhost` with the default port (`5672`) and credentials (`guest` / `guest`).


## Code

Run each example from this directory with `go run`:

[Tutorial one: "Hello World!"](https://www.rabbitmq.com/tutorials/tutorial-one-go.html):

    go run send.go
    go run receive.go

[Tutorial two: Work Queues](https://www.rabbitmq.com/tutorials/tutorial-two-go.html):

    go run new_task.go hello world
    go run worker.go

[Tutorial three: Publish/Subscribe](https://www.rabbitmq.com/tutorials/tutorial-three-go.html)

    go run receive_logs.go
    go run emit_log.go hello world

[Tutorial four: Routing](https://www.rabbitmq.com/tutorials/tutorial-four-go.html)

    go run receive_logs_direct.go info warn
    go run emit_log_direct.go warn "a warning"

[Tutorial five: Topics](https://www.rabbitmq.com/tutorials/tutorial-five-go.html)

    go run receive_logs_topic.go "kern.*" "*.critical"
    go run emit_log_topic.go kern.critical "A critical kernel error"

[Tutorial six: RPC](https://www.rabbitmq.com/tutorials/tutorial-six-go.html)

    go run rpc_server.go
    go run rpc_client.go 10

[Publisher confirms](https://www.rabbitmq.com/tutorials/tutorial-seven-java.html) (AMQP 1.0 publish outcomes)

    go run publisher_confirms.go

[AMQP 1.0 Direct Reply-To RPC](https://www.rabbitmq.com/docs/next/direct-reply-to)

    go run rpc_amqp10.go

To learn more, see the [package documentation](https://pkg.go.dev/github.com/rabbitmq/rabbitmq-amqp-go-client) and [AMQP in RabbitMQ](https://www.rabbitmq.com/docs/amqp).
