import com.rabbitmq.client.ConnectionFactory;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.Channel;
import com.rabbitmq.client.MessageProperties;

public class NewTask {
  public static void main(String[] argv) throws java.io.IOException {
    Connection connection = null;
    ConnectionFactory factory = new ConnectionFactory();
    factory.setHost("localhost");
    connection = factory.newConnection();
    Channel channel = connection.createChannel();
    
    channel.queueDeclare("task_queue", true, false, false, null);
    
    String message = getMessage(argv);
    
    channel.basicPublish( "", "task_queue", 
                MessageProperties.PERSISTENT_TEXT_PLAIN,
                message.getBytes());
    System.out.println(" [x] Sent '" + message + "'");
    channel.close();
    connection.close();
  }
    
  private static String getMessage(String[] strings){
    if (strings.length < 1)
      return "Hello World!";
    return joinStrings(strings);
  }  
  
  private static String joinStrings(String[] strings){
    String words = "";
    for (String astring: strings)
      words = words.concat(astring).concat(" ");
    return words.trim();
  }
}

