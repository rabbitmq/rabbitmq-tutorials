import com.rabbitmq.client.ConnectionFactory;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.Channel;
import com.rabbitmq.client.QueueingConsumer;

public class Recv {
  public static void main(String[] argv) {
    try {
      Connection conn = null;
      try {
        conn = new ConnectionFactory().newConnection();
        Channel chan = conn.createChannel();
        chan.queueDeclare("hello", false, false, false, null);
        QueueingConsumer consumer = new QueueingConsumer(chan);
        chan.basicConsume("hello", true, consumer);
        while (true) {
          QueueingConsumer.Delivery delivery = consumer.nextDelivery();
          System.out.println(new String(delivery.getBody()));
        }
      }
      finally {
        if (conn != null) conn.close();
      }
    }
    catch (Exception e) {
      System.err.println("Exception while consuming");
      e.printStackTrace();
    }
  }
}
