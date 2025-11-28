import dev.kourier.amqp.connection.amqpConfig
import dev.kourier.amqp.connection.createAMQPConnection
import dev.kourier.amqp.properties
import io.ktor.utils.io.core.*
import kotlinx.coroutines.*

suspend fun newTask(coroutineScope: CoroutineScope, message: String) {
    val config = amqpConfig {
        server {
            host = "localhost"
        }
    }
    val connection = createAMQPConnection(coroutineScope, config)
    val channel = connection.openChannel()

    // Declare a durable queue to survive broker restarts
    channel.queueDeclare(
        "task_queue",
        durable = true,        // Queue survives broker restart
        exclusive = false,
        autoDelete = false,
        arguments = emptyMap()
    )

    // Mark messages as persistent (deliveryMode = 2)
    val properties = properties {
        deliveryMode = 2u  // Persistent message
    }

    channel.basicPublish(
        message.toByteArray(),
        exchange = "",
        routingKey = "task_queue",
        properties = properties
    )
    println(" [x] Sent '$message'")

    channel.close()
    connection.close()
}

suspend fun worker(coroutineScope: CoroutineScope, workerName: String) {
    val config = amqpConfig {
        server {
            host = "localhost"
        }
    }
    val connection = createAMQPConnection(coroutineScope, config)
    val channel = connection.openChannel()

    // Declare the same durable queue
    channel.queueDeclare(
        "task_queue",
        durable = true,
        exclusive = false,
        autoDelete = false,
        arguments = emptyMap()
    )
    println(" [$workerName] Waiting for messages. To exit press CTRL+C")

    // Fair dispatch: don't give more than one message to a worker at a time
    channel.basicQos(count = 1u, global = false)

    // Consume with manual acknowledgment (noAck = false)
    val consumer = channel.basicConsume("task_queue", noAck = false)

    for (delivery in consumer) {
        val message = delivery.message.body.decodeToString()
        println(" [$workerName] Received '$message'")

        try {
            // Simulate work - each dot represents 1 second of work
            doWork(message)
            println(" [$workerName] Done")
        } finally {
            // Manual acknowledgment - message is removed from queue
            channel.basicAck(delivery.message, multiple = false)
        }
    }

    channel.close()
    connection.close()
}

/**
 * Simulates time-consuming work.
 * Each dot in the task string represents 1 second of work.
 */
private suspend fun doWork(task: String) {
    for (ch in task) {
        if (ch == '.') {
            delay(1000) // Sleep for 1 second per dot
        }
    }
}
