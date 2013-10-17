# Go code for RabbitMQ tutorials

Here you can find Go code examples from
[RabbitMQ tutorials](http://www.rabbitmq.com/getstarted.html).

## Requirements

To run this code you need [Go RabbitMQ client](https://github.com/streadway/amqp).

## Code

Code examples are executed via `go run`:

[Tutorial one: "Hello World!"](http://www.rabbitmq.com/tutorial-one-python.html):

    go run send.go
    go run receive.go

[Tutorial two: Work Queues](http://www.rabbitmq.com/tutorial-two-python.html):

    go run new_task.go hello world
    go run worker.go

[Tutorial three: Publish/Subscribe](http://www.rabbitmq.com/tutorial-three-python.html)

    go run receive_logs.go hello world
    go run emit_log.go

[Tutorial four: Routing](http://www.rabbitmq.com/tutorial-four-python.html)

    go run receive_logs_direct.go info warn
    go run emit_log_direct.go warn "a warning"

[Tutorial five: Topics](http://www.rabbitmq.com/tutorial-five-python.html)

    go run receive_logs_topic.go info warn
    go run emit_log_topic.go warn "a warning"

[Tutorial six: RPC](http://www.rabbitmq.com/tutorial-six-python.html)

    TBD

To learn more, see [Go RabbitMQ client](https://github.com/streadway/amqp).
