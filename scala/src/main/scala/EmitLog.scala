import com.rabbitmq.client.ConnectionFactory

object EmitLog {

  private val EXCHANGE_NAME = "logs"

  def main(argv: Array[String]) {
    val factory = new ConnectionFactory()
    factory.setHost("localhost")
    val connection = factory.newConnection()
    val channel = connection.createChannel()
    channel.exchangeDeclare(EXCHANGE_NAME, "fanout")
    val message = getMessage(argv)
    channel.basicPublish(EXCHANGE_NAME, "", null, message.getBytes("UTF-8"))
    println(" [x] Sent '" + message + "'")
    channel.close()
    connection.close()
  }

  private def getMessage(strings: Array[String]): String = {
    if (strings.length < 1) return "info: Hello World!"
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
