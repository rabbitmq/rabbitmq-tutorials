import com.rabbitmq.client.ConnectionFactory;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.Channel;
import java.io.IOException;

public class send {
  public static void main(String[] argv) {
    try {
      Connection conn = new ConnectionFactory().newConnection();
      Channel chan = conn.createChannel();
      chan.queueDeclare("hello", false, false, false, null);
      chan.basicPublish("", "hello", null, "Hello World!".getBytes());
      conn.close();
    }
    catch (IOException ioe) {
      System.err.println("IOException while publishing");
    }
  }
}
