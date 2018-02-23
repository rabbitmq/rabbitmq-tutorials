import com.rabbitmq.client.*;

import java.io.IOException;

public class RecvWithConnectionRecovery {

  private final static String QUEUE_NAME = "hello";

  public static void main(String[] argv) throws Exception {
    Address[] addresses = new Address[]{
        new Address("localhost", 5672),
        new Address("localhost", 5673)
    };
    ConnectionFactory factory = new ConnectionFactory();
    factory.setHost("localhost");
    factory.setAutomaticRecoveryEnabled(true);
    Connection connection = factory.newConnection(addresses);
    Channel channel = connection.createChannel();

    channel.queueDeclare(QUEUE_NAME, false, false, false, null);
    System.out.println(" [*] Waiting for messages. To exit press CTRL+C");

    Consumer consumer = new DefaultConsumer(channel) {
      @Override
      public void handleDelivery(String consumerTag, Envelope envelope, AMQP.BasicProperties properties, byte[] body)
          throws IOException {
        String message = new String(body, "UTF-8");
        System.out.println(" [x] Received '" + message + "'");
      }
    };
    channel.basicConsume(QUEUE_NAME, true, consumer);
  }
}
