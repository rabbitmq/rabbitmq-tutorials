import com.rabbitmq.client.Address;
import com.rabbitmq.client.Channel;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.ConnectionFactory;
import com.rabbitmq.client.Consumer;
import com.rabbitmq.client.DefaultConsumer;
import com.rabbitmq.client.Envelope;
import com.rabbitmq.client.AMQP.BasicProperties;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;

public class RecvWithConnectionRecovery {

  private static final Logger log = LoggerFactory.getLogger(RecvWithConnectionRecovery.class);
  private static final String QUEUE_NAME = "hello";

  public static void main(String[] argv) throws Exception {
    final Address[] addresses = new Address[]{
        new Address("localhost", 5672),
        new Address("localhost", 5673)
    };

    log.debug("Initializing ConnectionFactory");
    final ConnectionFactory factory = new ConnectionFactory();
    factory.setAutomaticRecoveryEnabled(true);
    factory.setTopologyRecoveryEnabled(true);

    log.debug("Creating Connection");
    final Connection connection = factory.newConnection(addresses);

    log.debug("Creating Channel");
    final Channel channel = connection.createChannel();

    channel.queueDeclare(QUEUE_NAME, false, false, false, null);
    log.info(" [*] Waiting for messages. To exit press CTRL+C");

    Consumer consumer = new DefaultConsumer(channel) {
      @Override
      public void handleDelivery(String consumerTag, Envelope envelope, BasicProperties properties, byte[] body)
          throws IOException {
        String message = new String(body, "UTF-8");
        System.out.println(" [x] Received '" + message + "'");
      }
    };
    channel.basicConsume(QUEUE_NAME, true, consumer);
  }
}
