import com.rabbitmq.client.*

/**
 * Author: vincent
 * Date: 2018-06-08 17:25:00
 * Comment:
 */

class ReceiveLogs {
    companion object {
        const val EXCHANGE_NAME = "logs"
    }
}

fun main(argv: Array<String>) {
    val factory = ConnectionFactory()
    factory.host = "localhost"
    val connection = factory.newConnection()
    val channel = connection.createChannel()

    channel.exchangeDeclare(ReceiveLogs.EXCHANGE_NAME, BuiltinExchangeType.FANOUT)
    val queueName = channel.queueDeclare().queue
    channel.queueBind(queueName, ReceiveLogs.EXCHANGE_NAME, "")

    println(" [*] Waiting for messages. To exit press CTRL+C")

    val consumer = object : DefaultConsumer(channel) {
        override fun handleDelivery(consumerTag: String, envelope: Envelope,
                                    properties: AMQP.BasicProperties, body: ByteArray) {
            val message = String(body, charset("UTF-8"))
            println(" [x] Received '$message'")
        }
    }
    channel.basicConsume(queueName, true, consumer)
}