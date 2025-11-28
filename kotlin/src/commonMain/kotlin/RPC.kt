import dev.kourier.amqp.connection.amqpConfig
import dev.kourier.amqp.connection.createAMQPConnection
import dev.kourier.amqp.properties
import io.ktor.utils.io.core.*
import kotlinx.coroutines.*
import kotlin.uuid.Uuid

/**
 * Fibonacci function - calculates Fibonacci number recursively.
 * Note: This is a simple recursive implementation for demonstration.
 * Not suitable for large numbers in production.
 */
private fun fib(n: Int): Int {
    return when {
        n == 0 -> 0
        n == 1 -> 1
        else -> fib(n - 1) + fib(n - 2)
    }
}

/**
 * RPC Server - Processes Fibonacci requests and sends responses.
 */
suspend fun rpcServer(coroutineScope: CoroutineScope) {
    val config = amqpConfig {
        server {
            host = "localhost"
        }
    }
    val connection = createAMQPConnection(coroutineScope, config)
    val channel = connection.openChannel()

    try {
        // Declare the RPC queue
        channel.queueDeclare(
            "rpc_queue",
            durable = false,
            exclusive = false,
            autoDelete = false,
            arguments = emptyMap()
        )

        // Fair dispatch - don't give more than one message at a time
        channel.basicQos(count = 1u, global = false)

        println(" [x] Awaiting RPC requests")

        // Consume RPC requests
        val consumer = channel.basicConsume("rpc_queue", noAck = false)

        for (delivery in consumer) {
            try {
                val props = delivery.message.properties
                val correlationId = props.correlationId
                val replyTo = props.replyTo

                // Parse request (expecting an integer)
                val requestMessage = delivery.message.body.decodeToString()
                val n = requestMessage.toIntOrNull() ?: 0

                println(" [.] fib($n)")

                // Calculate Fibonacci
                val response = fib(n)

                // Build response properties with correlation ID
                val replyProps = properties {
                    this.correlationId = correlationId
                }

                // Send response to the callback queue
                if (replyTo != null) {
                    channel.basicPublish(
                        response.toString().toByteArray(),
                        exchange = "",
                        routingKey = replyTo,
                        properties = replyProps
                    )
                }

                // Acknowledge the request
                channel.basicAck(delivery.message, multiple = false)
            } catch (e: Exception) {
                println(" [.] Error processing request: ${e.message}")
                e.printStackTrace()
                channel.basicAck(delivery.message, multiple = false)
            }
        }
    } finally {
        // Ensure cleanup happens even when coroutine is cancelled
        channel.close()
        connection.close()
    }
}

/**
 * RPC Client - Sends Fibonacci requests and waits for responses.
 */
suspend fun rpcClient(coroutineScope: CoroutineScope, n: Int): Int {
    val config = amqpConfig {
        server {
            host = "localhost"
        }
    }
    val connection = createAMQPConnection(coroutineScope, config)
    val channel = connection.openChannel()

    try {
        // Create an exclusive callback queue for receiving responses
        val callbackQueueDeclared = channel.queueDeclare(
            name = "",
            durable = false,
            exclusive = true,  // Exclusive to this connection
            autoDelete = true,
            arguments = emptyMap()
        )
        val callbackQueueName = callbackQueueDeclared.queueName

        // Generate a unique correlation ID for this request
        val correlationId = Uuid.random().toString()

        // Start consuming BEFORE sending the request to avoid race condition
        val consumer = channel.basicConsume(callbackQueueName, noAck = true)
        var result = 0

        // Build request properties
        val requestProps = properties {
            this.correlationId = correlationId
            this.replyTo = callbackQueueName
        }

        // Send the RPC request
        channel.basicPublish(
            n.toString().toByteArray(),
            exchange = "",
            routingKey = "rpc_queue",
            properties = requestProps
        )
        println(" [x] Requesting fib($n)")

        withTimeout(10000) { // 10 second timeout
            for (delivery in consumer) {
                val responseCorrelationId = delivery.message.properties.correlationId

                if (responseCorrelationId == correlationId) {
                    // Found matching response
                    val responseMessage = delivery.message.body.decodeToString()
                    result = responseMessage.toInt()
                    println(" [.] Got $result")
                    break
                }
            }
        }

        return result
    } finally {
        // Ensure cleanup happens even on timeout or error
        channel.close()
        connection.close()
    }
}
