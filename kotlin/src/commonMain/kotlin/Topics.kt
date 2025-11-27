import dev.kourier.amqp.BuiltinExchangeType
import dev.kourier.amqp.Properties
import dev.kourier.amqp.connection.amqpConfig
import dev.kourier.amqp.connection.createAMQPConnection
import io.ktor.utils.io.core.*
import kotlinx.coroutines.*

suspend fun emitLogTopic(coroutineScope: CoroutineScope, routingKey: String, message: String) {
    val config = amqpConfig {
        server {
            host = "localhost"
        }
    }
    val connection = createAMQPConnection(coroutineScope, config)
    val channel = connection.openChannel()

    // Declare a topic exchange - routes based on pattern matching
    channel.exchangeDeclare(
        "topic_logs",
        BuiltinExchangeType.TOPIC,
        durable = false,
        autoDelete = false,
        internal = false,
        arguments = emptyMap()
    )

    // Publish with a topic routing key (dot-separated words)
    channel.basicPublish(
        message.toByteArray(),
        exchange = "topic_logs",
        routingKey = routingKey,  // Format: <facility>.<severity> or similar
        properties = Properties()
    )
    println(" [x] Sent '$routingKey':'$message'")

    channel.close()
    connection.close()
}

suspend fun receiveLogsTopic(coroutineScope: CoroutineScope, subscriberName: String, bindingKeys: List<String>) {
    val config = amqpConfig {
        server {
            host = "localhost"
        }
    }
    val connection = createAMQPConnection(coroutineScope, config)
    val channel = connection.openChannel()

    // Declare the same topic exchange
    channel.exchangeDeclare(
        "topic_logs",
        BuiltinExchangeType.TOPIC,
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

    // Bind the queue with topic patterns
    for (bindingKey in bindingKeys) {
        channel.queueBind(
            queue = queueName,
            exchange = "topic_logs",
            routingKey = bindingKey  // Pattern with wildcards: * or #
        )
        println(" [$subscriberName] Binding queue to pattern '$bindingKey'")
    }
    println(" [$subscriberName] Waiting for messages matching ${bindingKeys.joinToString(", ")}")

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
