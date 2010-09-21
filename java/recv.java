import com.rabbitmq.client.ConnectionFactory;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.Channel;
import com.rabbitmq.client.QueueingConsumer;

import java.io.IOException;

public class recv {
  public static void main(String[] argv) {
    try {
      Connection conn = new ConnectionFactory().newConnection();
      Channel chan = conn.createChannel();
      chan.queueDeclare("hello", false, false, false, null);
      QueueingConsumer consumer = new QueueingConsumer(chan);
      chan.basicConsume("hello", true, consumer);
      while (true) {
        QueueingConsumer.Delivery delivery = consumer.nextDelivery();
        System.out.println(new String(delivery.getBody()));
      }
    }
    catch (IOException ioe) {
      System.err.println("IOException while consuming");
    }
    catch (InterruptedException ie) {
      System.err.println("InterruptedException while consuming");
    }
  }
}
