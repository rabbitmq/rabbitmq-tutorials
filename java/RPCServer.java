import com.rabbitmq.client.ConnectionFactory;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.Channel;
import com.rabbitmq.client.QueueingConsumer;
import com.rabbitmq.client.AMQP.BasicProperties;
  
public class RPCServer {
  
  private static final String RPC_QUEUE_NAME = "rpc_queue";
  
  private static int fib(int n) throws Exception {
    if (n > 1) return fib(n-1) + fib(n-2);
    else return n;
  }
    
  public static void main(String[] argv) {
    Connection connection = null;
    try {
      ConnectionFactory factory = new ConnectionFactory();
      factory.setHost("localhost");
  
      connection = factory.newConnection();
      Channel channel = connection.createChannel();
      
      channel.queueDeclare(RPC_QUEUE_NAME, false, false, false, null);
  
      channel.basicQos(1);
  
      QueueingConsumer consumer = new QueueingConsumer(channel);
      channel.basicConsume(RPC_QUEUE_NAME, false, consumer);
  
      System.out.println(" [x] Awaiting RPC requests");
  
      while (true) {
        String response = null;
        
        QueueingConsumer.Delivery delivery = consumer.nextDelivery();
        
        BasicProperties props = delivery.getProperties();
        BasicProperties replyProps = new BasicProperties();
        replyProps.setCorrelationId(props.getCorrelationId());        
        
        try {
          String message = new String(delivery.getBody());
          int n = Integer.parseInt(message);
  
          System.out.println(" [.] fib(" + message + ")");
          response = "" + fib(n);
        }
        catch (Exception e){
          System.out.println(" [.] " + e.toString());
          response = "";
        }
        finally {  
          channel.basicPublish( "", props.getReplyTo(), replyProps, response.getBytes());
  
          channel.basicAck(delivery.getEnvelope().getDeliveryTag(), false);
        }
      }
    }
    catch  (Exception e) {
      e.printStackTrace();
    }
    finally {
      if (connection != null) {
        try {
          connection.close();
        }
        catch (Exception ignore) {}
      }
    }      		      
  }
}

