import com.rabbitmq.client.AMQP.BasicProperties
import com.rabbitmq.client.{Channel, Connection, ConnectionFactory, QueueingConsumer}

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
      val consumer = new QueueingConsumer(channel)
      channel.basicConsume(RPC_QUEUE_NAME, false, consumer)
      println(" [x] Awaiting RPC requests")
      while (true) {
        var response: String = null
        val delivery = consumer.nextDelivery()
        val props = delivery.getProperties
        val replyProps = new BasicProperties.Builder().correlationId(props.getCorrelationId)
          .build()
        try {
          val message = new String(delivery.getBody, "UTF-8")
          val n = java.lang.Integer.parseInt(message)
          println(" [.] fib(" + message + ")")
          response = "" + fib(n)
        } catch {
          case e: Exception => {
            println(" [.] " + e.toString)
            response = ""
          }
        } finally {
          channel.basicPublish("", props.getReplyTo, replyProps, response.getBytes("UTF-8"))
          channel.basicAck(delivery.getEnvelope.getDeliveryTag, false)
        }
      }
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

  private def fib(n: Int): Int = {
    if (n == 0) return 0
    if (n == 1) return 1
    fib(n - 1) + fib(n - 2)
  }
}
