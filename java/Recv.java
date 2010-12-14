import com.rabbitmq.client.ConnectionFactory;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.Channel;
import com.rabbitmq.client.QueueingConsumer;

public class Recv {
    public static void main(String[] argv)
        throws java.io.IOException,
               java.lang.InterruptedException {
    Connection conn = null;
    ConnectionFactory factory = new ConnectionFactory();
    factory.setHost("localhost");
    conn = factory.newConnection();
    Channel chan = conn.createChannel();

    chan.queueDeclare("hello", false, false, false, null);

    System.out.println(" [*] Waiting for messages. To exit press CTRL+C");
    QueueingConsumer consumer = new QueueingConsumer(chan);
    chan.basicConsume("hello", true, consumer);
    while (true) {
      QueueingConsumer.Delivery delivery = consumer.nextDelivery();
      System.out.println(" [x] Received " + new String(delivery.getBody()));
    }
  }
}
