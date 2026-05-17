# Node.js code for RabbitMQ tutorials

Here you can find JavaScript (Node) code examples from [RabbitMQ
tutorials](https://www.rabbitmq.com/getstarted.html).

To successfully use the examples you will need a running RabbitMQ server.

## Requirements

### Node.js

You need [Node.js](https://nodejs.org/en/download/) and [amqplib](https://github.com/amqp-node/amqplib)
to run these tutorials.


### Client Library

To install `amqplib` using npm:

``` shell
npm install amqplib -g
```

## Code

[Tutorial one: "Hello World!"](https://www.rabbitmq.com/tutorials/tutorial-one-javascript.html):

``` shell
node src/send.js
node src/receive.js
```

[Tutorial two: Work Queues](https://www.rabbitmq.com/tutorials/tutorial-two-javascript.html):

``` shell
node src/new_task.js "A very hard task which takes two seconds.."
node src/worker.js
```

[Tutorial three: Publish/Subscribe](https://www.rabbitmq.com/tutorials/tutorial-three-javascript.html)

``` shell
node src/receive_logs.js
node src/emit_log.js "info: This is the log message"
```

[Tutorial four: Routing](https://www.rabbitmq.com/tutorials/tutorial-four-javascript.html):

``` shell
node src/receive_logs_direct.js info
node src/emit_log_direct.js info "The message"
```


[Tutorial five: Topics](https://www.rabbitmq.com/tutorials/tutorial-five-javascript.html):

``` shell
node src/receive_logs_topic.js "*.rabbit"
node src/emit_log_topic.js red.rabbit Hello
```

[Tutorial six: RPC](https://www.rabbitmq.com/tutorials/tutorial-six-javascript.html):

``` shell
node src/rpc_server.js
node src/rpc_client.js 30
```

## Running with Docker Compose (optional)

If you don't want to install RabbitMQ locally, you can alternatively run the
tutorials inside containers using the included `docker-compose.yml`:

``` shell
docker compose up --build send receive
```

This starts a RabbitMQ broker (with the management UI on
[http://localhost:15672](http://localhost:15672), default `guest`/`guest`
credentials) and runs Tutorial 1 (`send` and `receive`).

To run other tutorials with the same images, override the command on one of
the existing services, for example:

``` shell
docker compose run --rm send node src/new_task.js "A very hard task..."
docker compose run --rm receive node src/worker.js
```

The source files honor an optional `AMQP_URL` environment variable, falling
back to `amqp://localhost`, so the same code runs unchanged against either a
local broker or the broker started by Compose.

To tear everything down, including the broker's volume:

``` shell
docker compose down -v
```
