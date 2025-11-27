# RabbitMQ Tutorials in Kotlin (JVM & Native)

This repository contains RabbitMQ tutorials implemented using [Kourier](https://kourier.dev), a pure Kotlin multiplatform AMQP client.

## About

These tutorials demonstrate the core concepts of RabbitMQ using Kotlin and the Kourier AMQP client library. All examples work on JVM, macOS (ARM64), Linux (x64), and Windows (x64) platforms through Kotlin Multiplatform.

## Prerequisites

- RabbitMQ server running on localhost (default port 5672)
- Kotlin 2.1.21
- Gradle

## Building

```bash
./gradlew build
```

## Tutorials

### 1. Hello World (HelloWorld.kt)

The simplest thing that does something - sending and receiving messages from a named queue.

Functions:
- `send()` - Sends "Hello World!" message to the queue
- `receive()` - Receives and prints messages from the queue

### 2. Work Queues (WorkQueues.kt)

Distributing time-consuming tasks among multiple workers.

Functions:
- `newTask(message)` - Sends a task to the work queue (dots represent work time)
- `worker(workerName)` - Processes tasks with fair dispatch and manual acknowledgment

Key concepts:
- Message durability
- Fair dispatch with `basicQos`
- Manual acknowledgments

### 3. Publish/Subscribe (PublishSubscribe.kt)

Sending messages to many consumers at once using fanout exchanges.

Functions:
- `emitLog(message)` - Publishes log messages to all subscribers
- `receiveLogs(subscriberName)` - Subscribes to all log messages

Key concepts:
- Fanout exchanges
- Temporary queues
- Broadcasting messages

### 4. Routing (Routing.kt)

Receiving messages selectively using direct exchanges and routing keys.

Functions:
- `emitLogDirect(severity, message)` - Publishes log with specific severity
- `receiveLogsDirect(subscriberName, severities)` - Subscribes to specific severities

Key concepts:
- Direct exchanges
- Routing keys
- Multiple bindings per queue

### 5. Topics (Topics.kt)

Receiving messages based on patterns using topic exchanges.

Functions:
- `emitLogTopic(routingKey, message)` - Publishes with topic routing key
- `receiveLogsTopic(subscriberName, bindingKeys)` - Subscribes using patterns

Key concepts:
- Topic exchanges
- Wildcard patterns (`*` = one word, `#` = zero or more words)
- Pattern-based routing

### 6. RPC (RPC.kt)

Request/reply pattern for remote procedure calls.

Functions:
- `rpcServer()` - Processes Fibonacci number requests
- `rpcClient(n)` - Sends RPC request and waits for response

Key concepts:
- Callback queues
- Correlation IDs
- Reply-to pattern

## Running Examples

These tutorials are designed as library functions. You can call them from your own code or tests. For example:

```kotlin
import kotlinx.coroutines.runBlocking

fun main() = runBlocking {
    // Example: Run work queue tutorial
    launch { worker(this, "Worker-1") }
    launch { worker(this, "Worker-2") }

    delay(1000) // Give workers time to start

    newTask(this, "Task with work...")
}
```

## More Information

For detailed explanations of each tutorial, see the [Kourier documentation](https://kourier.dev/tutorials/).
