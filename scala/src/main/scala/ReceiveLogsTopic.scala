import com.rabbitmq.client._

object ReceiveLogsTopic {

  private val EXCHANGE_NAME = "topic_logs"

  def main(argv: Array[String]) {
    val factory = new ConnectionFactory()
    factory.setHost("localhost")
    val connection = factory.newConnection()
    val channel = connection.createChannel()
    channel.exchangeDeclare(EXCHANGE_NAME, "topic")
    val queueName = channel.queueDeclare().getQueue
    if (argv.length < 1) {
      System.err.println("Usage: ReceiveLogsTopic [binding_key]...")
      System.exit(1)
    }
    for (bindingKey <- argv) {
      channel.queueBind(queueName, EXCHANGE_NAME, bindingKey)
    }
    println(" [*] Waiting for messages. To exit press CTRL+C")
    val consumer = new DefaultConsumer(channel) {

      override def handleDelivery(consumerTag: String,
                                  envelope: Envelope,
                                  properties: AMQP.BasicProperties,
                                  body: Array[Byte]) {
        val message = new String(body, "UTF-8")
        println(" [x] Received '" + envelope.getRoutingKey + "':'" + message +
          "'")
      }
    }
    channel.basicConsume(queueName, true, consumer)
  }
}
