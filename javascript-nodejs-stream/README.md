# Node.js code for RabbitMQ stream tutorials

Here you can find JavaScript (Node) code examples from [RabbitMQ tutorials](https://www.rabbitmq.com/getstarted.html) related to the [Stream plugin](https://www.rabbitmq.com/docs/stream).

To successfully use the examples you will need a running RabbitMQ server with the [stream plugin enabled](https://www.rabbitmq.com/docs/stream#enabling-plugin).

See [First Application With RabbitMQ Streams](https://www.rabbitmq.com/blog/2021/07/19/rabbitmq-streams-first-application), [Stream plugin documentation](https://www.rabbitmq.com/docs/stream) and [how to preconfigure plugins](https://www.rabbitmq.com/docs/plugins#enabled-plugins-file).

## Requirements

Apart from [Node.js](https://nodejs.org/en/download/), these examples use the [`rabbitmq-stream-js-client`](https://github.com/coders51/rabbitmq-stream-js-client) client library.

## Code

Code examples are executed via `npm`:

[Tutorial one: "Hello World!"](https://www.rabbitmq.com/tutorials/tutorial-one-javascript-stream):

```shell
npm run send
npm run receive
```

[Tutorial two: Offset Tracking](https://www.rabbitmq.com/tutorials/tutorial-two-javascript-stream):

```shell
npm run offset-tracking-publish
npm run offset-tracking-receive
```

To learn more, see [`coders51/rabbitmq-stream-js-client`](https://github.com/coders51/rabbitmq-stream-js-client).
