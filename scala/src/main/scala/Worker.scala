import com.rabbitmq.client._

object Worker {

  private val TASK_QUEUE_NAME = "task_queue"

  def main(argv: Array[String]) {
    val factory = new ConnectionFactory()
    factory.setHost("localhost")
    val connection = factory.newConnection()
    val channel = connection.createChannel()
    channel.queueDeclare(TASK_QUEUE_NAME, true, false, false, null)
    println(" [*] Waiting for messages. To exit press CTRL+C")
    channel.basicQos(1)
    val consumer = new DefaultConsumer(channel) {

      override def handleDelivery(consumerTag: String,
                                  envelope: Envelope,
                                  properties: AMQP.BasicProperties,
                                  body: Array[Byte]) {
        val message = new String(body, "UTF-8")
        println(" [x] Received '" + message + "'")
        try {
          doWork(message)
        } finally {
          println(" Done")
          channel.basicAck(envelope.getDeliveryTag, false)
        }
      }
    }
    channel.basicConsume(TASK_QUEUE_NAME, false, consumer)
  }

  private def doWork(task: String) {
    print(" [x] Processing ")

    for (ch <- task.toCharArray() if ch == '.') {
      try {
        print(".")
        Thread.sleep(1000)
      } catch {
        case _ignored: InterruptedException => Thread.currentThread().interrupt()
      }
    }
  }
}
