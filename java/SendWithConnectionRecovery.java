import com.rabbitmq.client.Address;
import com.rabbitmq.client.Channel;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.ConnectionFactory;

public class SendWithConnectionRecovery {

  private final static String QUEUE_NAME = "hello";

  public static void main(String[] argv) throws Exception {
    Address[] addresses = new Address[]{
        new Address("localhost", 5672),
        new Address("localhost", 5673)
    };
    ConnectionFactory factory = new ConnectionFactory();
    factory.setAutomaticRecoveryEnabled(true);
    Connection connection = factory.newConnection(addresses);
    Channel channel = connection.createChannel();

    channel.queueDeclare(QUEUE_NAME, false, false, false, null);
    String message = "Hello World!";
    channel.basicPublish("", QUEUE_NAME, null, message.getBytes("UTF-8"));
    System.out.println(" [x] Sent '" + message + "'");

    channel.close();
    connection.close();
  }
}
