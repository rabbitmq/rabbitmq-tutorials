# Agent Notes: Node.js Pipeline Example

This port is a Docker Compose-oriented Node.js pipeline example. Unlike the
numbered `javascript-nodejs` tutorials, the scripts connect to
`amqp://rabbitmq` because they are intended to run inside the Compose network.

## Running the Example

Use Docker Compose for end-to-end checks:

```sh
docker compose up --build consumer worker producer
```

The `producer` exits after sending its batch. The `worker` and `consumer` are
long-running services.

Manual `node` runs require the hostname `rabbitmq` to resolve to a running
RabbitMQ broker.

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
