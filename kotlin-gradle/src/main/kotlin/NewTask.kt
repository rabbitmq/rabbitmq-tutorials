import com.rabbitmq.client.ConnectionFactory
import com.rabbitmq.client.MessageProperties


class NewTask {
    companion object {
        const val TASK_QUEUE_NAME = "task_queue"

        fun getMessage(strings: Array<String>): String {
            return if (strings.isEmpty()) "Hello World!" else joinStrings(strings, " ")
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

fun main(argv: Array<String>) {
    val factory = ConnectionFactory()
    factory.host = "localhost"
    val connection = factory.newConnection()
    val channel = connection.createChannel()

    channel.queueDeclare(NewTask.TASK_QUEUE_NAME, true, false, false, null)

    val message = NewTask.getMessage(argv)

    channel.basicPublish("", NewTask.TASK_QUEUE_NAME,
            MessageProperties.PERSISTENT_TEXT_PLAIN,
            message.toByteArray(charset("UTF-8")))
    println(" [x] Sent '$message'")

    channel.close()
    connection.close()
}