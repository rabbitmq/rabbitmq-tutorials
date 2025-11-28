import dev.kourier.amqp.Properties
import dev.kourier.amqp.connection.amqpConfig
import dev.kourier.amqp.connection.createAMQPConnection
import io.ktor.utils.io.core.*
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.delay
import kotlinx.coroutines.launch
import kotlinx.coroutines.runBlocking

val queueName = "hello"

suspend fun send(coroutineScope: CoroutineScope) {
    val config = amqpConfig {
        server {
            host = "localhost"
        }
    }
    val connection = createAMQPConnection(coroutineScope, config)
    val channel = connection.openChannel()

    channel.queueDeclare(queueName, durable = false, exclusive = false, autoDelete = false, arguments = emptyMap())
    val message = "Hello World!"
    channel.basicPublish(message.toByteArray(), exchange = "", routingKey = queueName, properties = Properties())
    println("[x] Sent '$message'")

    channel.close()
    connection.close()
}

suspend fun receive(coroutineScope: CoroutineScope) {
    val config = amqpConfig {
        server {
            host = "localhost"
        }
    }
    val connection = createAMQPConnection(coroutineScope, config)
    val channel = connection.openChannel()

    channel.queueDeclare(queueName, durable = false, exclusive = false, autoDelete = false, arguments = emptyMap())
    println("[*] Waiting for messages. To exit press CTRL+C")

    val consumer = channel.basicConsume(queueName, noAck = true)
    for (delivery in consumer) {
        val message: String = delivery.message.body.decodeToString()
        println("[x] Received '$message'")
    }

    channel.close()
    connection.close()
}

fun main() = runBlocking {
    val coroutineScope = this

    launch { send(coroutineScope) }
    launch { receive(coroutineScope) }

    delay(Long.MAX_VALUE) // Keep the main thread alive to allow the consumer to run
}

