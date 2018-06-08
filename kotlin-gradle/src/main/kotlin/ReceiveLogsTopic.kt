import com.rabbitmq.client.*

/**
 * Author: vincent
 * Date: 2018-06-08 17:28:00
 * Comment:
 */

class ReceiveLogsTopic {
    companion object {
        const val EXCHANGE_NAME = "topic_logs"
    }
}

fun main(argv: Array<String>) {
    val factory = ConnectionFactory()
    factory.host = "localhost"
    val connection = factory.newConnection()
    val channel = connection.createChannel()

    channel.exchangeDeclare(ReceiveLogsTopic.EXCHANGE_NAME, BuiltinExchangeType.TOPIC)
    val queueName = channel.queueDeclare().queue

    if (argv.isEmpty()) {
        System.err.println("Usage: ReceiveLogsTopic [binding_key]...")
        System.exit(1)
    }

    for (bindingKey in argv) {
        channel.queueBind(queueName, ReceiveLogsTopic.EXCHANGE_NAME, bindingKey)
    }

    println(" [*] Waiting for messages. To exit press CTRL+C")

    val consumer = object : DefaultConsumer(channel) {
        override fun handleDelivery(consumerTag: String, envelope: Envelope,
                                    properties: AMQP.BasicProperties, body: ByteArray) {
            val message = String(body, charset("UTF-8"))
            System.out.println(" [x] Received '" + envelope.routingKey + "':'" + message + "'")
        }
    }
    channel.basicConsume(queueName, true, consumer)
}