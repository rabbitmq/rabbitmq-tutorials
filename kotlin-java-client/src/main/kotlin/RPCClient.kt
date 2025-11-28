import com.rabbitmq.client.*
import java.util.*
import java.util.concurrent.ArrayBlockingQueue


class RPCClient {
    private var connection: Connection
    private var channel: Channel
    private val requestQueueName = "rpc_queue"
    private var replyQueueName: String

    init {
        val factory = ConnectionFactory()
        factory.host = "localhost"
        connection = factory.newConnection()
        channel = connection.createChannel()
        replyQueueName = channel.queueDeclare().queue
    }

    fun call(message: String): String {
        val corrId = UUID.randomUUID().toString()

        val props = AMQP.BasicProperties.Builder()
                .correlationId(corrId)
                .replyTo(replyQueueName)
                .build()

        channel.basicPublish("", requestQueueName, props, message.toByteArray(charset("UTF-8")))

        val response = ArrayBlockingQueue<String>(1)

        channel.basicConsume(replyQueueName, true, object : DefaultConsumer(channel) {
            override fun handleDelivery(consumerTag: String, envelope: Envelope, properties: AMQP.BasicProperties, body: ByteArray) {
                if (properties.correlationId == corrId) {
                    response.offer(String(body, charset("UTF-8")))
                }
            }
        })

        return response.take()
    }

    fun close() {
        connection.close()
    }
}

fun main(args: Array<String>) {
    val fibonacciRpc = RPCClient()
    val response: String = fibonacciRpc.call("30")
    println(" [x] Requesting fib(30)")
    println(" [.] Got '$response'")
    fibonacciRpc.close()
}