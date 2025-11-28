import dev.kourier.amqp.BuiltinExchangeType
import dev.kourier.amqp.Properties
import dev.kourier.amqp.connection.amqpConfig
import dev.kourier.amqp.connection.createAMQPConnection
import io.ktor.utils.io.core.*
import kotlinx.coroutines.*

suspend fun emitLogDirect(coroutineScope: CoroutineScope, severity: String, message: String) {
    val config = amqpConfig {
        server {
            host = "localhost"
        }
    }
    val connection = createAMQPConnection(coroutineScope, config)
    val channel = connection.openChannel()

    // Declare a direct exchange - routes based on exact routing key match
    channel.exchangeDeclare(
        "direct_logs",
        BuiltinExchangeType.DIRECT,
        durable = false,
        autoDelete = false,
        internal = false,
        arguments = emptyMap()
    )

    // Publish with severity as the routing key
    channel.basicPublish(
        message.toByteArray(),
        exchange = "direct_logs",
        routingKey = severity,  // Routing key determines which queues receive the message
        properties = Properties()
    )
    println(" [x] Sent '$severity':'$message'")

    channel.close()
    connection.close()
}

suspend fun receiveLogsDirect(coroutineScope: CoroutineScope, subscriberName: String, severities: List<String>) {
    val config = amqpConfig {
        server {
            host = "localhost"
        }
    }
    val connection = createAMQPConnection(coroutineScope, config)
    val channel = connection.openChannel()

    // Declare the same direct exchange
    channel.exchangeDeclare(
        "direct_logs",
        BuiltinExchangeType.DIRECT,
        durable = false,
        autoDelete = false,
        internal = false,
        arguments = emptyMap()
    )

    // Declare a temporary queue
    val queueDeclared = channel.queueDeclare(
        name = "",
        durable = false,
        exclusive = true,
        autoDelete = true,
        arguments = emptyMap()
    )
    val queueName = queueDeclared.queueName
    println(" [$subscriberName] Created temporary queue: $queueName")

    // Bind the queue to the exchange with each severity
    for (severity in severities) {
        channel.queueBind(
            queue = queueName,
            exchange = "direct_logs",
            routingKey = severity  // Only receive messages with this routing key
        )
        println(" [$subscriberName] Binding queue to '$severity'")
    }
    println(" [$subscriberName] Waiting for ${severities.joinToString(", ")} logs. To exit press CTRL+C")

    // Consume messages
    val consumer = channel.basicConsume(queueName, noAck = true)

    for (delivery in consumer) {
        val routingKey = delivery.message.routingKey
        val message = delivery.message.body.decodeToString()
        println(" [$subscriberName] Received '$routingKey':'$message'")
    }

    channel.close()
    connection.close()
}
