import com.rabbitmq.client.ConnectionFactory;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.Channel;
import java.io.IOException;

public class Send {
  public static void main(String[] argv) {
    try {
      Connection conn = null;
      try {
        ConnectionFactory factory = new ConnectionFactory();
        factory.setHost("localhost");
        conn = factory.newConnection();
        Channel chan = conn.createChannel();
        chan.queueDeclare("hello", false, false, false, null);
        chan.basicPublish("", "hello", null, "Hello World!".getBytes());
      }
      finally {
        if (conn != null) conn.close();
      }
    }
    catch (Exception e) {
      System.err.println("Exception while publishing");
      e.printStackTrace();
    }
  }
}
