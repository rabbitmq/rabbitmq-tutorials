import com.rabbitmq.client.ConnectionFactory;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.Channel;
import com.rabbitmq.client.QueueingConsumer;
import com.rabbitmq.client.AMQP.BasicProperties;
import java.util.UUID;
    
public class RPCClient {
    
  static class FibonacciRpcClient {
    private Connection connection;
    private Channel channel;
    private String requestQueueName = "rpc_queue";
    private String replyQueueName;
    private QueueingConsumer consumer;
    
    public FibonacciRpcClient() throws Exception {
      ConnectionFactory factory = new ConnectionFactory();
      factory.setHost("localhost");
      connection = factory.newConnection();
      channel = connection.createChannel();

      replyQueueName = channel.queueDeclare().getQueue(); 
      consumer = new QueueingConsumer(channel);
      channel.basicConsume(replyQueueName, true, consumer);
    }
  
    public String call(String message) throws Exception {     
      String response = null;
      boolean replied = false;
      String corrId = UUID.randomUUID().toString();
    
      BasicProperties props = new BasicProperties();
      props.setReplyTo(replyQueueName);
      props.setCorrelationId(corrId);
    
      channel.basicPublish("", requestQueueName, props, message.getBytes());
    
      while (replied == false) {
        QueueingConsumer.Delivery delivery = consumer.nextDelivery();
        if (delivery.getProperties().getCorrelationId().compareTo(corrId) == 0) {
          response = new String(delivery.getBody());
          replied = true;
        }
      }

      return response; 
    }
    
    public void close() throws Exception {
      channel.close();
      connection.close();
    }
  }
  
  public static void main(String[] argv) {
    RPCClient.FibonacciRpcClient fibonacciRpc = null;
    String response = null;
    try {
      fibonacciRpc = new RPCClient.FibonacciRpcClient();
  
      System.out.println(" [x] Requesting fib(30)");   
      response = fibonacciRpc.call("30");
      System.out.println(" [.] Got '" + response + "'");
      System.out.println(" [x] Requesting fib(-1)");   
      response = fibonacciRpc.call("-1");
      System.out.println(" [.] Got '" + response + "'");
      System.out.println(" [x] Requesting fib(a)");   
      response = fibonacciRpc.call("a");
      System.out.println(" [.] Got '" + response + "'");            
    }
    catch  (Exception e) {
      e.printStackTrace();
    }
    finally {
      if (fibonacciRpc!= null) {
        try {
          fibonacciRpc.close();
        }
        catch (Exception ignore) {}
      }
    }
  }
}

