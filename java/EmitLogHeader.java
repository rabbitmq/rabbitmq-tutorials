import java.util.Map;
import java.util.HashMap;
import com.rabbitmq.client.ConnectionFactory;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.Channel;
import com.rabbitmq.client.MessageProperties;
import com.rabbitmq.client.AMQP.BasicProperties;
import com.rabbitmq.client.*;

public class EmitLogHeader {

  private static final String EXCHANGE_NAME = "header_test";

  public static void main(String[] argv) {
    Connection connection = null;
    Channel channel = null;
    try {
    
      if (argv.length < 1){
        System.err.println("Usage: EmitLogHeader message queueName [headers]...");
        System.exit(1);
      }

      // The API requires a routing key, but in fact if you are using a header exchange the
      // value of the routing key is not used in the routing. You can store information
      // for the receiver here as the routing key is still available in the received message.
      String routingKey = "ourTestRoutingKey";

      // Argument processing: the first arg is the message, the rest are
      // key value pairs for headers.
      String message = argv[0];

      // The map for the headers.
      Map<String, Object> headers = new HashMap<String, Object>();

      // The rest of the arguments are key value header pairs.  For the purpose of this
      // example, we are assuming they are all strings, but that is not required by RabbitMQ
      for(int i = 1; i < argv.length; i++)  {
        System.out.println("Adding header " + argv[i] + " with value " + argv[i + 1] + " to Map");
        headers.put(argv[i], argv[i+1]);
        i++;
      }

      ConnectionFactory factory = new ConnectionFactory();
      factory.setHost("localhost");
  
      connection = factory.newConnection();
      channel = connection.createChannel();

      channel.exchangeDeclare(EXCHANGE_NAME, "headers");

      AMQP.BasicProperties.Builder builder = new AMQP.BasicProperties.Builder();

      // MessageProperties.PERSISTENT_TEXT_PLAIN is a static instance of AMQP.BasicProperties 
      // that contains a delivery mode and a priority. So we pass them to the builder.
      builder.deliveryMode(MessageProperties.PERSISTENT_TEXT_PLAIN.getDeliveryMode());    
      builder.priority(MessageProperties.PERSISTENT_TEXT_PLAIN.getPriority());    

      // Add the headers to the builder.
      builder.headers(headers);

      // Use the builder to create the BasicProperties object.
      AMQP.BasicProperties theProps = builder.build();

      // Now we add the headers.  This example only uses string headers, but they can also be integers
      channel.basicPublish(EXCHANGE_NAME, routingKey, theProps, message.getBytes());
      System.out.println(" [x] Sent message: '" + message + "'");

    }
    catch  (Exception e) {
      e.printStackTrace();
    }
    finally {
      if (connection != null) {
        try {
          connection.close();
        }
        catch (Exception ignore) {}
      }
    }
  }
}

