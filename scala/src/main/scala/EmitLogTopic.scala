import com.rabbitmq.client.ConnectionFactory

object EmitLogTopic {

  private val EXCHANGE_NAME = "topic_logs"

  def main(argv: Array[String]) {
    val factory = new ConnectionFactory()
    factory.setHost("localhost")
    val connection = factory.newConnection()
    val channel = connection.createChannel()
    channel.exchangeDeclare(EXCHANGE_NAME, "topic")
    val routingKey = getRouting(argv)
    val message = getMessage(argv)
    channel.basicPublish(EXCHANGE_NAME, routingKey, null, message.getBytes("UTF-8"))
    println(" [x] Sent '" + routingKey + "':'" + message + "'")
    channel.close()
    connection.close()
  }

  private def getRouting(strings: Array[String]): String = {
    if (strings.length < 1) return "anonymous.info"
    strings(0)
  }

  private def getMessage(strings: Array[String]): String = {
    if (strings.length < 2) return "Hello World!"
    joinStrings(strings, " ", 1)
  }

  private def joinStrings(strings: Array[String], delimiter: String, startIndex: Int): String = {
    val length = strings.length
    if (length == 0) return ""
    if (length < startIndex) return ""
    val words = new StringBuilder(strings(startIndex))
    for (i <- startIndex + 1 until length) {
      words.append(delimiter).append(strings(i))
    }
    words.toString
  }
}
