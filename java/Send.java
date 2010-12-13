import com.rabbitmq.client.ConnectionFactory;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.Channel;
import java.io.IOException;

public class Send {
  public static void main(String[] argv)
      throws java.io.IOException {
    Connection conn = null;
    ConnectionFactory factory = new ConnectionFactory();
    factory.setHost("localhost");
    conn = factory.newConnection();
    Channel chan = conn.createChannel();

    chan.queueDeclare("hello", false, false, false, null);

    chan.basicPublish("", "hello", null, "Hello World!".getBytes());
    System.out.println(" [x] Sent 'Hello World!'");
    conn.close();
  }
}
