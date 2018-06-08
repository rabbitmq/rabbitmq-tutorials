import com.rabbitmq.client.AMQP
import com.rabbitmq.client.ConnectionFactory
import com.rabbitmq.client.DefaultConsumer
import com.rabbitmq.client.Envelope
import sun.jvm.hotspot.HelloWorld.fib


/**
 * Author: vincent
 * Date: 2018-06-08 17:35:00
 * Comment:
 */

class RPCServer {
    companion object {
        const val RPC_QUEUE_NAME = "rpc_queue"

        fun fib(n: Int): Int {
            if (n == 0) return 0
            return if (n == 1) 1 else fib(n - 1) + fib(n - 2)
        }
    }
}

fun main(args: Array<String>) {
    val factory = ConnectionFactory()
    factory.host = "localhost"
    val connection = factory.newConnection()
    val channel = connection.createChannel()

    channel.queueDeclare(RPCServer.RPC_QUEUE_NAME, false, false, false, null)

    channel.basicQos(1)

    System.out.println(" [x] Awaiting RPC requests")

    val consumer = object : DefaultConsumer(channel) {
        override fun handleDelivery(consumerTag: String, envelope: Envelope, properties: AMQP.BasicProperties, body: ByteArray) {
            val replyProps = AMQP.BasicProperties.Builder()
                    .correlationId(properties.correlationId)
                    .build()
            val message = String(body, charset("UTF-8"))
            val n = Integer.parseInt(message)
            println(" [.] fib($message)")
            val response = fib(n).toString()
            channel.basicPublish("", properties.replyTo, replyProps, response.toByteArray())
            channel.basicAck(envelope.deliveryTag, false)
        }
    }

    channel.basicConsume(RPCServer.RPC_QUEUE_NAME, false, consumer)
    // Wait and be prepared to consume the message from RPC client.
    while (true) {
        synchronized(consumer) {
            (consumer as Object).wait()
        }
    }
}