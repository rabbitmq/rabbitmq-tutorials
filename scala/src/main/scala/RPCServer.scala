import java.util.concurrent.CountDownLatch

import com.rabbitmq.client.AMQP.BasicProperties
import com.rabbitmq.client._

class ServerConsumer(val ch: Channel, val latch: CountDownLatch) extends DefaultConsumer(ch) {
  override def handleDelivery(consumerTag: String, envelope: Envelope, properties: BasicProperties, body: Array[Byte]): Unit = {
    var response: String = null
    val replyProps = new BasicProperties.Builder()
          .correlationId(properties.getCorrelationId)
          .build

    try {
          val message = new String(body, "UTF-8")
          val n = java.lang.Integer.parseInt(message)
          println(" [.] fib(" + message + ")")
          response = "" + Fibonacci.fib(n)
        } catch {
          case e: Exception => {
            println(" [.] " + e.toString)
            response = ""
          }
        } finally {
          ch.basicPublish("", properties.getReplyTo, replyProps, response.getBytes("UTF-8"))
          ch.basicAck(envelope.getDeliveryTag, false)
          latch.countDown()
        }
  }
}

object Fibonacci {
   def fib(n: Int): Int = {
    if (n == 0) return 0
    if (n == 1) return 1
    fib(n - 1) + fib(n - 2)
  }
}

object RPCServer {
  private val RPC_QUEUE_NAME = "rpc_queue"

  def main(argv: Array[String]) {
    var connection: Connection = null
    var channel: Channel = null
    try {
      val factory = new ConnectionFactory()
      factory.setHost("localhost")
      connection = factory.newConnection()
      channel = connection.createChannel()
      channel.queueDeclare(RPC_QUEUE_NAME, false, false, false, null)
      channel.basicQos(1)
      // stop after one consumed message since this is example code
      val latch = new CountDownLatch(1)
      val consumer = new ServerConsumer(channel, latch)
      channel.basicConsume(RPC_QUEUE_NAME, false, consumer)
      println(" [x] Awaiting RPC requests")
      latch.await()
    } catch {
      case e: Exception => e.printStackTrace()
    } finally {
      if (connection != null) {
        try {
          connection.close()
        } catch {
          case ignore: Exception =>
        }
      }
    }
  }
}
