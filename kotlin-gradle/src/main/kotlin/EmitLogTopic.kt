
import com.rabbitmq.client.BuiltinExchangeType
import com.rabbitmq.client.Channel
import com.rabbitmq.client.Connection
import com.rabbitmq.client.ConnectionFactory

/**
 * Author: vincent
 * Date: 2018-06-08 17:13:00
 * Comment:
 */

class EmitLogTopic {
    companion object {
        const val EXCHANGE_NAME = "topic_logs"

        fun getRouting(strings: Array<String>): String {
            return if (strings.isEmpty()) "anonymous.info" else strings[0]
        }

        fun getMessage(strings: Array<String>): String {
            return if (strings.size < 2) "Hello World!" else joinStrings(strings, " ", 1)
        }

        private fun joinStrings(strings: Array<String>, delimiter: String, startIndex: Int): String {
            val length = strings.size
            if (length == 0) return ""
            if (length < startIndex) return ""
            val words = StringBuilder(strings[startIndex])
            for (i in startIndex + 1 until length) {
                words.append(delimiter).append(strings[i])
            }
            return words.toString()
        }
    }
}

fun main(argv: Array<String>) {
    val factory = ConnectionFactory()
    factory.host = "localhost"
    val connection: Connection = factory.newConnection()
    val channel: Channel = connection.createChannel()

    channel.exchangeDeclare(EmitLogTopic.EXCHANGE_NAME, BuiltinExchangeType.TOPIC)

    val routingKey = EmitLogTopic.getRouting(argv)
    val message = EmitLogTopic.getMessage(argv)

    channel.basicPublish(EmitLogTopic.EXCHANGE_NAME, routingKey, null, message.toByteArray(charset("UTF-8")))
    println(" [x] Sent '$routingKey':'$message'")
}