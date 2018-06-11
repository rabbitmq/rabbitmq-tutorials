import com.rabbitmq.client.AMQP
import com.rabbitmq.client.ConnectionFactory
import com.rabbitmq.client.DefaultConsumer
import com.rabbitmq.client.Envelope
import java.io.IOException


class Worker {
    companion object {
        const val TASK_QUEUE_NAME = "task_queue"

        fun doWork(task: String) {
            for (ch in task.toCharArray()) {
                if (ch == '.') {
                    try {
                        Thread.sleep(1000)
                    } catch (_ignored: InterruptedException) {
                        Thread.currentThread().interrupt()
                    }
                }
            }
        }
    }
}

fun main(argv: Array<String>) {
    val factory = ConnectionFactory()
    factory.host = "localhost"
    val connection = factory.newConnection()
    val channel = connection.createChannel()

    channel.queueDeclare(Worker.TASK_QUEUE_NAME, true, false, false, null)
    println(" [*] Waiting for messages. To exit press CTRL+C")

    channel.basicQos(1)

    val consumer = object : DefaultConsumer(channel) {
        @Throws(IOException::class)
        override fun handleDelivery(consumerTag: String, envelope: Envelope, properties: AMQP.BasicProperties, body: ByteArray) {
            val message = String(body, charset("UTF-8"))

            println(" [x] Received '$message'")
            try {
                Worker.doWork(message)
            } finally {
                println(" [x] Done")
                channel.basicAck(envelope.deliveryTag, false)
            }
        }
    }
    channel.basicConsume(Worker.TASK_QUEUE_NAME, false, consumer)
}