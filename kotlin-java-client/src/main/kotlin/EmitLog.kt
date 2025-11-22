import com.rabbitmq.client.BuiltinExchangeType
import com.rabbitmq.client.ConnectionFactory


class EmitLog {
    companion object {
        const val EXCHANGE_NAME = "logs"

        fun getMessage(strings: Array<String>): String {
            return if (strings.isEmpty()) "info: Hello World!" else joinStrings(strings, " ")
        }

        private fun joinStrings(strings: Array<String>, delimiter: String): String {
            val length = strings.size
            if (length == 0) return ""
            val words = StringBuilder(strings[0])
            for (i in 1 until length) {
                words.append(delimiter).append(strings[i])
            }
            return words.toString()
        }
    }
}

fun main(args: Array<String>) {
    val factory = ConnectionFactory()
    factory.host = "localhost"
    val connection = factory.newConnection()
    val channel = connection.createChannel()
    channel.exchangeDeclare(EmitLog.EXCHANGE_NAME, BuiltinExchangeType.FANOUT)

    val message = EmitLog.getMessage(args)
    channel.basicPublish(EmitLog.EXCHANGE_NAME, "", null, message.toByteArray())
    System.out.println(" [x] Sent '$message'")

    channel.close()
    connection.close()
}

