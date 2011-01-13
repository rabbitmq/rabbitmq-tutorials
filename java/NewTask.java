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

        boolean durable = true;
        channel.queueDeclare("task_queue", durable, false, false, null);

        String message = joinStrings(argv);
        if (message == "") message = "Hello World!";

        // make message props persistent
        channel.basicPublish( "", "task_queue", 
                MessageProperties.PERSISTENT_TEXT_PLAIN, 
                message.getBytes());
        System.out.println(" [x] Sent '" + message + "'");
        channel.close();
        connection.close();
    }
  
  private static String joinStrings(String[] strings){
     String words = "";
     for (String astring: strings)
    	words = words + astring + " ";
     return words.trim();
  }
}
