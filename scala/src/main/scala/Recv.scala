
import com.rabbitmq.client._

object Recv {

  private val QUEUE_NAME = "hello"

  def main(argv: Array[String]) {
    val factory = new ConnectionFactory()
    factory.setHost("localhost")
    val connection = factory.newConnection()
    val channel = connection.createChannel()
    channel.queueDeclare(QUEUE_NAME, false, false, false, null)
    println(" [*] Waiting for messages. To exit press CTRL+C")
    val consumer = new DefaultConsumer(channel) {

      override def handleDelivery(consumerTag: String,
                                  envelope: Envelope,
                                  properties: AMQP.BasicProperties,
                                  body: Array[Byte]) {
        var message = new String(body, "UTF-8")
        println(" [x] Received '" + message + "'")
      }
    }
    channel.basicConsume(QUEUE_NAME, true, consumer)
  }
}
