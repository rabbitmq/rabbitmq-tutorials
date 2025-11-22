
import com.rabbitmq.client.BuiltinExchangeType
import com.rabbitmq.client.ConnectionFactory


class EmitLogDirect {
    companion object {
        const val EXCHANGE_NAME = "direct_logs"

        fun getSeverity(strings: Array<String>): String {
            return if (strings.isEmpty()) "info" else strings[0]
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
    val connection = factory.newConnection()
    val channel = connection.createChannel()

    channel.exchangeDeclare(EmitLogDirect.EXCHANGE_NAME, BuiltinExchangeType.DIRECT)

    val severity = EmitLogDirect.getSeverity(argv)
    val message = EmitLogDirect.getMessage(argv)

    channel.basicPublish(EmitLogDirect.EXCHANGE_NAME, severity, null, message.toByteArray(charset("UTF-8")))
    println(" [x] Sent '$severity':'$message'")

    channel.close()
    connection.close()
}