import com.rabbitmq.client._

object ReceiveLogs {

  private val EXCHANGE_NAME = "logs"

  def main(argv: Array[String]) {
    val factory = new ConnectionFactory()
    factory.setHost("localhost")

    val connection = factory.newConnection()
    val channel = connection.createChannel()
    channel.exchangeDeclare(EXCHANGE_NAME, "fanout")
    val queueName = channel.queueDeclare().getQueue
    channel.queueBind(queueName, EXCHANGE_NAME, "")
    println(" [*] Waiting for messages. To exit press CTRL+C")
    val consumer = new DefaultConsumer(channel) {

      override def handleDelivery(consumerTag: String,
                                  envelope: Envelope,
                                  properties: AMQP.BasicProperties,
                                  body: Array[Byte]) {
        val message = new String(body, "UTF-8")
        println(" [x] Received '" + message + "'")
      }
    }
    channel.basicConsume(queueName, true, consumer)
  }
}
