# Agent Notes: Node.js Pipeline Pattern

This port demonstrates a process that is both a RabbitMQ consumer and producer.
Use it as the reference pattern when implementing a simple message pipeline:

```text
producer -> pipeline.input -> worker -> pipeline.output -> consumer
```

The worker consumes from `pipeline.input`, transforms each message, publishes
the transformed message to `pipeline.output`, waits for broker confirmation,
and only then acknowledges the original delivery.

## Running the Example

Use Docker Compose for end-to-end checks:

```sh
docker compose up --build consumer worker producer
```

The `producer` exits after sending its batch. The `worker` and `consumer` are
long-running services.

Manual `node` runs require the hostname `rabbitmq` to resolve to a running
RabbitMQ broker. The scripts intentionally use `amqp://rabbitmq` because the
primary workflow is Docker Compose.

## Implementing the Pattern

 * Declare both input and output queues before consuming or publishing
 * Use durable quorum queues for `pipeline.input` and `pipeline.output`
 * Use manual acknowledgements on the input consumer
 * Set `prefetch(1)` so each worker handles one unacknowledged delivery at a time
 * Publish the transformed output message with `persistent: true`
 * Use a confirm channel for publish-confirm-then-ack behavior
 * Acknowledge the input delivery only after the output publish has been confirmed
 * On publish failure, `nack` the input delivery with requeue enabled
 * Keep the transformation small and visible in the worker

This pattern preserves the original message until the broker has accepted the
derived message. It can still produce duplicates if a worker crashes after the
output publish is confirmed but before the input acknowledgement reaches the
broker, so consumers should be prepared for duplicate messages in real systems.

## Code Guidelines

 * Keep the three scripts self-contained and close to the style of the numbered Node.js tutorials
 * Keep `amqp://rabbitmq` unless the README and Compose setup are updated together
 * Preserve the worker's publish-confirm-then-ack flow
 * Use durable quorum queues for `pipeline.input` and `pipeline.output`
 * Keep the transformation simple so the example stays focused on the pipeline pattern
 * Avoid adding dependencies unless they are essential to the tutorial

## Documentation

 * Keep the README focused on runnable commands and the pipeline behavior
 * Keep Docker Compose instructions first because they match the hard-coded broker hostname
 * Do not add full stops to Markdown list items
