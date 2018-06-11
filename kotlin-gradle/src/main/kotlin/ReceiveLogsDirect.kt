import com.rabbitmq.client.*


class ReceiveLogsDirect {
    companion object {
        const val EXCHANGE_NAME = "direct_logs"
    }
}

fun main(argv: Array<String>) {
    val factory = ConnectionFactory()
    factory.host = "localhost"
    val connection = factory.newConnection()
    val channel = connection.createChannel()

    channel.exchangeDeclare(ReceiveLogsDirect.EXCHANGE_NAME, BuiltinExchangeType.DIRECT)
    val queueName = channel.queueDeclare().queue

    if (argv.isEmpty()) {
        System.err.println("Usage: ReceiveLogsDirect [info] [warning] [error]")
        System.exit(1)
    }

    for (severity in argv) {
        channel.queueBind(queueName, ReceiveLogsDirect.EXCHANGE_NAME, severity)
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
