import dev.kourier.amqp.BuiltinExchangeType
import dev.kourier.amqp.Properties
import dev.kourier.amqp.connection.amqpConfig
import dev.kourier.amqp.connection.createAMQPConnection
import io.ktor.utils.io.core.*
import kotlinx.coroutines.*

suspend fun emitLog(coroutineScope: CoroutineScope, message: String) {
    val config = amqpConfig {
        server {
            host = "localhost"
        }
    }
    val connection = createAMQPConnection(coroutineScope, config)
    val channel = connection.openChannel()

    // Declare a fanout exchange - broadcasts to all bound queues
    channel.exchangeDeclare(
        "logs",
        BuiltinExchangeType.FANOUT,
        durable = false,
        autoDelete = false,
        internal = false,
        arguments = emptyMap()
    )

    // Publish to the exchange (routing key is ignored for fanout)
    channel.basicPublish(
        message.toByteArray(),
        exchange = "logs",
        routingKey = "",  // Routing key is ignored by fanout exchanges
        properties = Properties()
    )
    println(" [x] Sent '$message'")

    channel.close()
    connection.close()
}

suspend fun receiveLogs(coroutineScope: CoroutineScope, subscriberName: String) {
    val config = amqpConfig {
        server {
            host = "localhost"
        }
    }
    val connection = createAMQPConnection(coroutineScope, config)
    val channel = connection.openChannel()

    // Declare the same fanout exchange
    channel.exchangeDeclare(
        "logs",
        BuiltinExchangeType.FANOUT,
        durable = false,
        autoDelete = false,
        internal = false,
        arguments = emptyMap()
    )

    // Declare a temporary, exclusive, auto-delete queue
    // The server generates a unique name for us
    val queueDeclared = channel.queueDeclare(
        name = "",           // Empty name = server generates a unique name
        durable = false,
        exclusive = true,    // Queue is deleted when connection closes
        autoDelete = true,   // Queue is deleted when no consumers
        arguments = emptyMap()
    )
    val queueName = queueDeclared.queueName
    println(" [$subscriberName] Created temporary queue: $queueName")

    // Bind the queue to the exchange
    channel.queueBind(
        queue = queueName,
        exchange = "logs",
        routingKey = ""  // Routing key is ignored for fanout
    )
    println(" [$subscriberName] Waiting for logs. To exit press CTRL+C")

    // Consume messages with auto-ack (since these are just logs)
    val consumer = channel.basicConsume(queueName, noAck = true)

    for (delivery in consumer) {
        val message = delivery.message.body.decodeToString()
        println(" [$subscriberName] $message")
    }

    channel.close()
    connection.close()
}
