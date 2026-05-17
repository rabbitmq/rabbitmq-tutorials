# Hybrid Consumer-and-Producer pipeline (Node.js)

A self-contained Node.js example showing a process that is **both a consumer
and a producer** on the same channel. The worker reads messages from one
queue, transforms each one, republishes the result to a second queue, and
only then acknowledges the input message — so a message is never dropped in
transit and never silently re-delivered after a successful republish.

This is a complementary example to the numbered RabbitMQ tutorials, focused
on the hybrid-node pattern that sits at the heart of most real pipelines,
fan-out workers, and message routers.

## The pattern

```
producer --> pipeline.input --> worker (consumer + producer) --> pipeline.output --> consumer
```

 - `producer` publishes input messages into `pipeline.input`
 - `worker` consumes from `pipeline.input` and, for each delivery, publishes a transformed message to `pipeline.output` before acknowledging the input
 - `consumer` reads from `pipeline.output`

`pipeline.input` and `pipeline.output` are both declared as durable quorum
queues, so the worker can be restarted at any point without losing in-flight
work.

## How the worker behaves

 - `prefetch(1)` — one unacknowledged delivery at a time, so the worker never holds more than one message in memory and load is naturally spread across multiple worker replicas
 - **Publish-then-ack** — the input delivery is acknowledged *after* the transformed message has been handed to `sendToQueue`. If the worker crashes mid-transform, the original message is redelivered by the broker
 - **Persistent output** — the republished message is marked persistent and lands in a quorum queue, so it survives broker restarts

The transformation used here is intentionally trivial (uppercase the body)
so the focus stays on the topology and the acknowledgement flow.

## Files

 - `worker.js` — the hybrid consumer-and-producer
 - `producer.js` — publishes a fixed batch of messages to `pipeline.input` and exits
 - `consumer.js` — subscribes to `pipeline.output` and prints what arrives

## Requirements

[Node.js](https://nodejs.org/en/download/) and the
[`amqplib`](https://github.com/amqp-node/amqplib) client. The example
connects to RabbitMQ at `amqp://localhost`, matching the style of the
numbered Node.js tutorials in [`../javascript-nodejs`](../javascript-nodejs).

## Run against a local RabbitMQ

Install dependencies once:

``` shell
npm install
```

Then, in three terminals (start the consumer and the worker before the
producer so the live output is easier to follow):

``` shell
node consumer.js
node worker.js
node producer.js
```

## Run under Docker Compose

The included `docker-compose.yml` defines a RabbitMQ broker (with the
management UI on [http://localhost:15672](http://localhost:15672), default
`guest`/`guest` credentials) and the three pipeline services.

To stand the whole pipeline up:

``` shell
docker compose up --build consumer worker producer
```

To tear everything down, including the broker's volume:

``` shell
docker compose down -v
```
