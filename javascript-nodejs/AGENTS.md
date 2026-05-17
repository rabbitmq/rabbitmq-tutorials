# Agent Notes: Node.js Tutorials

## Broker connection

`src/*.js` calls `amqp.connect(process.env.AMQP_URL || 'amqp://localhost')`.
Keep that fallback when adding new tutorial files so both the standard
`localhost` flow and the optional Docker Compose flow keep working.

## Docker Compose

`docker-compose.yml` is an optional convenience for running the tutorials in
containers. Do not bind broker ports to `0.0.0.0`; keep them on `127.0.0.1`.
The canonical path remains a RabbitMQ node on `localhost` with default
settings, as required by the repo-wide `AGENTS.md`.
