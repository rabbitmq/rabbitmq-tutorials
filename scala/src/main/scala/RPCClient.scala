import java.util.UUID
import java.util.concurrent.{ArrayBlockingQueue, BlockingQueue}

import com.rabbitmq.client.AMQP.BasicProperties
import com.rabbitmq.client._

class ResponseCallback(val corrId: String) extends DeliverCallback {
  val response: BlockingQueue[String] = new ArrayBlockingQueue[String](1)

  override def handle(consumerTag: String, message: Delivery): Unit = {
    if (message.getProperties.getCorrelationId.equals(corrId)) {
      response.offer(new String(message.getBody, "UTF-8"))
    }
  }

  def take(): String = {
    response.take();
  }
}

class RPCClient(host: String) {

  val factory = new ConnectionFactory()
  factory.setHost(host)

  val connection: Connection = factory.newConnection()
  val channel: Channel = connection.createChannel()
  val requestQueueName: String = "rpc_queue"
  val replyQueueName: String = channel.queueDeclare().getQueue

  def call(message: String): String = {
    val corrId = UUID.randomUUID().toString
    val props = new BasicProperties.Builder().correlationId(corrId)
      .replyTo(replyQueueName)
      .build()
    channel.basicPublish("", requestQueueName, props, message.getBytes("UTF-8"))

    val responseCallback = new ResponseCallback(corrId)
    channel.basicConsume(replyQueueName, true, responseCallback, _ => { })

    responseCallback.take()
  }

  def close() {
    connection.close()
  }
}

object RPCClient {

  def main(argv: Array[String]) {
    var fibonacciRpc: RPCClient = null
    var response: String = null
    try {
      val host = if (argv.isEmpty) "localhost" else argv(0)

      fibonacciRpc = new RPCClient(host)
      println(" [x] Requesting fib(30)")
      response = fibonacciRpc.call("30")
      println(" [.] Got '" + response + "'")
    } catch {
      case e: Exception => e.printStackTrace()
    } finally {
      if (fibonacciRpc != null) {
        try {
          fibonacciRpc.close()
        } catch {
          case ignore: Exception =>
        }
      }
    }
  }
}
