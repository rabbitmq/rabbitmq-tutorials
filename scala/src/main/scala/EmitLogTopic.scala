import com.rabbitmq.client.{Channel, Connection, ConnectionFactory}

object EmitLogTopic {

  private val EXCHANGE_NAME = "topic_logs"

  def main(argv: Array[String]) {
    var connection: Connection = null
    var channel: Channel = null
    try {
      val factory = new ConnectionFactory()
      factory.setHost("localhost")
      connection = factory.newConnection()
      channel = connection.createChannel()
      channel.exchangeDeclare(EXCHANGE_NAME, "topic")
      val routingKey = getRouting(argv)
      val message = getMessage(argv)
      channel.basicPublish(EXCHANGE_NAME, routingKey, null, message.getBytes("UTF-8"))
      println(" [x] Sent '" + routingKey + "':'" + message + "'")
    } catch {
      case e: Exception => e.printStackTrace()
    } finally {
      if (connection != null) {
        try {
          connection.close()
        } catch {
          case ignore: Exception =>
        }
      }
    }
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
