import java.util.HashMap

import com.rabbitmq.client._
//remove if not needed

object EmitLogHeader {

  private val EXCHANGE_NAME = "header_test"

  def main(argv: Array[String]) {
    if (argv.length < 1) {
      System.err.println("Usage: EmitLogHeader message queueName [headers]...")
      System.exit(1)
    }
    val routingKey = "ourTestRoutingKey"
    val message = argv(0)
    val headers = new HashMap[String, Object]()
    for (i <- 1 until argv.length by 2) {
      println("Adding header " + argv(i) + " with value " + argv(i + 1) +
        " to Map")
      headers.put(argv(i), argv(i + 1))
    }
    val factory = new ConnectionFactory()
    factory.setHost("localhost")

    val connection = factory.newConnection()
    val channel = connection.createChannel()
    channel.exchangeDeclare(EXCHANGE_NAME, "headers")
    val builder = new AMQP.BasicProperties.Builder()
    builder.deliveryMode(MessageProperties.PERSISTENT_TEXT_PLAIN.getDeliveryMode)
    builder.priority(MessageProperties.PERSISTENT_TEXT_PLAIN.getPriority)
    builder.headers(headers)
    val theProps = builder.build()
    channel.basicPublish(EXCHANGE_NAME, routingKey, theProps, message.getBytes("UTF-8"))
    println(" [x] Sent message: '" + message + "'")
    channel.close()
    connection.close()
  }
}
