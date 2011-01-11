import com.rabbitmq.client.ConnectionFactory;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.Channel;
import com.rabbitmq.client.MessageProperties;

public class NewTask {
  public static void main(String[] argv)
      throws java.io.IOException {
    Connection conn = null;
    ConnectionFactory factory = new ConnectionFactory();
    factory.setHost("localhost");
    conn = factory.newConnection();
    Channel chan = conn.createChannel();

     // make durable
    chan.queueDeclare("task_queue", true, false, false, null);

    String message = joinStrings(argv);
    if (message == "") message = "Hello World!";

     // make persistent
    chan.basicPublish("", "task_queue", MessageProperties.PERSISTENT_TEXT_PLAIN, message.getBytes());
    System.out.println(" [x] Sent '" + message + "'");
    chan.close();
    conn.close();
  }
  
  private static String joinStrings(String[] strings){
     String words = "";
     for (String astring: strings)
    	words = words + astring + " ";
     return words.trim();
  }
}
