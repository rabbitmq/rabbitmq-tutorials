# Go code for RabbitMQ tutorials


Here you can find Go code examples from [RabbitMQ tutorials](https://www.rabbitmq.com/getstarted.html).


## Requirements

To run this code you need [Go RabbitMQ client](https://github.com/streadway/amqp):

    go get github.com/streadway/amqp


## Code

Code examples are executed via `go run`:

[Tutorial one: "Hello World!"](https://www.rabbitmq.com/tutorial-one-go.html):

    go run send.go
    go run receive.go

[Tutorial two: Work Queues](https://www.rabbitmq.com/tutorial-two-go.html):

    go run new_task.go hello world
    go run worker.go

[Tutorial three: Publish/Subscribe](https://www.rabbitmq.com/tutorial-three-go.html)

    go run receive_logs.go
    go run emit_log.go hello world

[Tutorial four: Routing](https://www.rabbitmq.com/tutorial-four-go.html)

    go run receive_logs_direct.go info warn
    go run emit_log_direct.go warn "a warning"

[Tutorial five: Topics](https://www.rabbitmq.com/tutorial-five-go.html)

    go run receive_logs_topic.go "kern.*" "*.critical"
    go run emit_log_topic.go kern.critical "A critical kernel error"

[Tutorial six: RPC](https://www.rabbitmq.com/tutorial-six-go.html)

    go run rpc_server.go
    go run rpc_client.go 10

To learn more, see [Go RabbitMQ client](https://github.com/streadway/amqp).
