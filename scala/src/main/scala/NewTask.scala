
import com.rabbitmq.client.{ConnectionFactory, MessageProperties}

object NewTask {

  private val TASK_QUEUE_NAME = "task_queue"

  def main(argv: Array[String]) {
    val factory = new ConnectionFactory()
    factory.setHost("localhost")
    val connection = factory.newConnection()
    val channel = connection.createChannel()
    channel.queueDeclare(TASK_QUEUE_NAME, true, false, false, null)
    val message = getMessage(argv)
    channel.basicPublish("", TASK_QUEUE_NAME, MessageProperties.PERSISTENT_TEXT_PLAIN, message.getBytes("UTF-8"))
    println(" [x] Sent '" + message + "'")
    channel.close()
    connection.close()
  }

  private def getMessage(strings: Array[String]): String = {
    if (strings.length < 1) return "Hello World!"
    joinStrings(strings, " ")
  }

  private def joinStrings(strings: Array[String], delimiter: String): String = {
    val length = strings.length
    if (length == 0) return ""
    val words = new StringBuilder(strings(0))
    for (i <- 1 until length) {
      words.append(delimiter).append(strings(i))
    }
    words.toString
  }
}
