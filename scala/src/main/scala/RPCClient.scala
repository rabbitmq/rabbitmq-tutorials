import java.util.UUID

import com.rabbitmq.client.AMQP.BasicProperties
import com.rabbitmq.client.{Channel, Connection, ConnectionFactory, QueueingConsumer}

class RPCClient(host: String) {

  val factory = new ConnectionFactory()
  factory.setHost(host)

  val connection: Connection = factory.newConnection()
  val channel: Channel = connection.createChannel()
  val requestQueueName: String = "rpc_queue"
  val replyQueueName: String = channel.queueDeclare().getQueue
  val consumer: QueueingConsumer = new QueueingConsumer(channel)

  channel.basicConsume(replyQueueName, true, consumer)

  def call(message: String): String = {
    var response: String = null
    val corrId = UUID.randomUUID().toString
    val props = new BasicProperties.Builder().correlationId(corrId)
      .replyTo(replyQueueName)
      .build()
    channel.basicPublish("", requestQueueName, props, message.getBytes("UTF-8"))
    while (response == null) {
      val delivery = consumer.nextDelivery()
      if (delivery.getProperties.getCorrelationId == corrId) {
        response = new String(delivery.getBody, "UTF-8")
      }
    }
    response
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
