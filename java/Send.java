import com.rabbitmq.client.ConnectionFactory;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.Channel;
import java.io.IOException;

public class Send {
  public static void main(String[] argv) {
    try {
      Connection conn = null;
      try {
        conn = new ConnectionFactory().newConnection();
        Channel chan = conn.createChannel();
        chan.queueDeclare("test", false, false, false, null);
        chan.basicPublish("", "test", null, "Hello World!".getBytes());
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
