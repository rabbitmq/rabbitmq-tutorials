# Node.js code for RabbitMQ tutorials

Here you can find Node.js code examples from [RabbitMQ
tutorials](http://www.rabbitmq.com/getstarted.html).

To successfully use the examples you will need a running RabbitMQ server.

## Requirements

Apart from `npm` and `node`, to run this code you need
[`node-amqp`](https://github.com/postwait/node-amqp) version 0.1.X. To
pull the dependency from `npm` run:

    npm install

## Code

[Tutorial one: "Hello World!"](http://www.rabbitmq.com/tutorial-one-python.html):

    node send.js
    node receive.js

[Tutorial three: Publish/Subscribe](http://www.rabbitmq.com/tutorial-three-python.html):

    node receive_logs.js
    node emit_log.js "info: This is the log message"
