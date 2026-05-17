# Hybrid Consumer-and-Producer pipeline (Node.js)

A self-contained Node.js example showing a process that is **both a consumer
and a producer** on the same channel. The worker reads messages from one
queue, transforms each one, republishes the result to a second queue, and
acknowledges the input message after the broker confirms the republished
message.

This is a complementary example to the numbered RabbitMQ tutorials, focused
on the hybrid-node pattern that sits at the heart of most real pipelines,
fan-out workers, and message routers.

## The pattern

```
producer --> pipeline.input --> worker (consumer + producer) --> pipeline.output --> consumer
```

 - `producer` publishes input messages into `pipeline.input`
 - `worker` consumes from `pipeline.input`, publishes a transformed message to `pipeline.output`, and then acknowledges the input
 - `consumer` reads from `pipeline.output`

`pipeline.input` and `pipeline.output` are both durable quorum queues.

## How the worker behaves

 - `prefetch(1)` — one unacknowledged delivery at a time, so the worker never holds more than one message in memory and load is spread across multiple worker replicas
 - **Publish-confirm-then-ack** — the input delivery is acknowledged *after* the transformed message has been confirmed by the broker. If the worker crashes mid-transform or before the confirm arrives, the broker redelivers the original message
 - **Persistent output** — the republished message is marked persistent and lands in a quorum queue, so it survives broker restarts

The transformation used here is intentionally trivial (uppercase the body)
so the focus stays on the topology and the acknowledgement flow.

## Files

 - `worker.js` — the hybrid consumer-and-producer
 - `producer.js` — publishes a fixed batch of messages to `pipeline.input` and exits
 - `consumer.js` — subscribes to `pipeline.output` and prints what arrives

## Requirements

### Docker Compose

To run the complete example, you need Docker with Docker Compose. The Compose
file starts RabbitMQ and all three pipeline processes.

### Manual Node.js run

To run the scripts outside Docker, you need [Node.js](https://nodejs.org/en/download/)
and the [`amqplib`](https://github.com/amqp-node/amqplib) client. The scripts
connect to RabbitMQ at `amqp://rabbitmq`, so that host name must resolve to a
running broker.

## Run under Docker Compose

The included `docker-compose.yml` defines a RabbitMQ broker (with the
management UI on [http://localhost:15672](http://localhost:15672), default
`guest`/`guest` credentials) and the three pipeline services.

To stand the whole pipeline up:

``` shell
docker compose up --build consumer worker producer
```

The producer publishes its batch and exits. The worker and consumer keep
running so you can inspect the queues or send another producer batch.

To tear everything down, including the broker's volume:

``` shell
docker compose down -v
```

## Run the Node.js scripts manually

Install dependencies once:

``` shell
npm install
```

Then, in three terminals, start the consumer and the worker before the
producer. This requires `rabbitmq` to resolve to the broker host:

``` shell
node consumer.js
node worker.js
node producer.js
```
