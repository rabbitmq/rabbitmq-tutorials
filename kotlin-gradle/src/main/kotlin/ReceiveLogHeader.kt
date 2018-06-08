import com.rabbitmq.client.*
import java.util.*


/**
 * Author: vincent
 * Date: 2018-06-08 17:19:00
 * Comment:
 */

class ReceiveLogHeader {
    companion object {
        const val EXCHANGE_NAME = "header_test"
    }
}

fun main(argv: Array<String>) {
    if (argv.isEmpty()) {
        System.err.println("Usage: ReceiveLogsHeader queueName [headers]...")
        System.exit(1)
    }

    val factory = ConnectionFactory()
    factory.host = "localhost"
    val connection = factory.newConnection()
    val channel = connection.createChannel()

    channel.exchangeDeclare(ReceiveLogHeader.EXCHANGE_NAME, BuiltinExchangeType.HEADERS)

    // The API requires a routing key, but in fact if you are using a header exchange the
    // value of the routing key is not used in the routing. You can receive information
    // from the sender here as the routing key is still available in the received message.
    val routingKeyFromUser = "ourTestRoutingKey"

    // Argument processing: the first arg is the local queue name, the rest are
    // key value pairs for headers.
    val queueInputName = argv[0]

    // The map for the headers.
    val headers = HashMap<String, Any>()

    // The rest of the arguments are key value header pairs.  For the purpose of this
    // example, we are assuming they are all strings, but that is not required by RabbitMQ
    // Note that when you run this code you should include the x-match header on the command
    // line. Example:
    //    java -cp $CP ReceiveLogsHeader testQueue1  x-match any header1 value1
    var i = 1
    while (i < argv.size) {
        headers[argv[i]] = argv[i + 1]
        println("Binding header " + argv[i] + " and value " + argv[i + 1] + " to queue " + queueInputName)
        i++
        i++
    }

    val queueName = channel.queueDeclare(queueInputName, true, false, false, null).queue
    channel.queueBind(queueName, ReceiveLogHeader.EXCHANGE_NAME, routingKeyFromUser, headers)

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
