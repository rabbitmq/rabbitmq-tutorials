import com.rabbitmq.client.ConnectionFactory

/**
 * Author: vincent
 * Date: 2018-06-08 17:47:00
 * Comment:
 */

class Send {
    companion object {
        const val QUEUE_NAME = "hello"
    }
}

fun main(argv: Array<String>) {
    val factory = ConnectionFactory()
    factory.host = "localhost"
    val connection = factory.newConnection()
    val channel = connection.createChannel()

    channel.queueDeclare(Send.QUEUE_NAME, false, false, false, null)
    val message = "Hello World!"
    channel.basicPublish("", Send.QUEUE_NAME, null, message.toByteArray(charset("UTF-8")))
    println(" [x] Sent '$message'")

    channel.close()
    connection.close()
}