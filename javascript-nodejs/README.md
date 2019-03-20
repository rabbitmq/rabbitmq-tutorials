# Node.js code for RabbitMQ tutorials

Here you can find JavaScript (Node) code examples from [RabbitMQ
tutorials](https://www.rabbitmq.com/getstarted.html).

To successfully use the examples you will need a running RabbitMQ server.

## Requirements

### Node.js

You need [Node.js](https://nodejs.org/en/download/) and [amqp.node](https://github.com/squaremo/amqp.node)
to run these tutorials.


### Client Library

To install `amqp.node` using npm:

    npm install amqplib -g

## Code

[Tutorial one: "Hello World!"](https://www.rabbitmq.com/tutorials/tutorial-one-javascript.html):

    node src/send.js
    node src/receive.js


[Tutorial two: Work Queues](https://www.rabbitmq.com/tutorials/tutorial-two-javascript.html):

    node src/new_task.js "A very hard task which takes two seconds.."
    node src/worker.js


[Tutorial three: Publish/Subscribe](https://www.rabbitmq.com/tutorials/tutorial-three-javascript.html)

    node src/receive_logs.js
    node src/emit_log.js "info: This is the log message"

[Tutorial four: Routing](https://www.rabbitmq.com/tutorials/tutorial-four-javascript.html):

    node src/receive_logs_direct.js info
    node src/emit_log_direct.js info "The message"


[Tutorial five: Topics](https://www.rabbitmq.com/tutorials/tutorial-five-javascript.html):

    node src/receive_logs_topic.js "*.rabbit"
    node src/emit_log_topic.js red.rabbit Hello

[Tutorial six: RPC](https://www.rabbitmq.com/tutorials/tutorial-six-javascript.html):

    node src/rpc_server.js
    node src/rpc_client.js
