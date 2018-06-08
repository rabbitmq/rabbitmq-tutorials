import com.rabbitmq.client.AMQP
import com.rabbitmq.client.BuiltinExchangeType
import com.rabbitmq.client.ConnectionFactory
import com.rabbitmq.client.MessageProperties
import java.util.*

/**
 * Author: vincent
 * Date: 2018-06-08 17:09:00
 * Comment:
 */

class EmitLogHeader {
    companion object {
        const val EXCHANGE_NAME = "header_test"
    }
}

fun main(argv: Array<String>) {
    if (argv.isEmpty()) {
        System.err.println("Usage: EmitLogHeader message queueName [headers]...")
        System.exit(1)
    }

    // The API requires a routing key, but in fact if you are using a header exchange the
    // value of the routing key is not used in the routing. You can store information
    // for the receiver here as the routing key is still available in the received message.
    val routingKey = "ourTestRoutingKey"

    // Argument processing: the first arg is the message, the rest are
    // key value pairs for headers.
    val message = argv[0]

    // The map for the headers.
    val headers = HashMap<String, Any>()

    // The rest of the arguments are key value header pairs.  For the purpose of this
    // example, we are assuming they are all strings, but that is not required by RabbitMQ
    var i = 1
    while (i < argv.size) {
        println("Adding header " + argv[i] + " with value " + argv[i + 1] + " to Map")
        headers[argv[i]] = argv[i + 1]
        i++
        i++
    }

    val factory = ConnectionFactory()
    factory.host = "localhost"
    val connection = factory.newConnection()
    val channel = connection.createChannel()

    channel.exchangeDeclare(EmitLogHeader.EXCHANGE_NAME, BuiltinExchangeType.HEADERS)

    val builder = AMQP.BasicProperties.Builder()

    // MessageProperties.PERSISTENT_TEXT_PLAIN is a static instance of AMQP.BasicProperties
    // that contains a delivery mode and a priority. So we pass them to the builder.
    builder.deliveryMode(MessageProperties.PERSISTENT_TEXT_PLAIN.deliveryMode)
    builder.priority(MessageProperties.PERSISTENT_TEXT_PLAIN.priority)

    // Add the headers to the builder.
    builder.headers(headers)

    // Use the builder to create the BasicProperties object.
    val theProps = builder.build()

    // Now we add the headers.  This example only uses string headers, but they can also be integers
    channel.basicPublish(EmitLogHeader.EXCHANGE_NAME, routingKey, theProps, message.toByteArray(charset("UTF-8")))
    println(" [x] Sent message: '$message'")
}