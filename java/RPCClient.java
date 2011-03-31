import java.io.IOException;
import com.rabbitmq.client.ConnectionFactory;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.Channel;
import com.rabbitmq.client.QueueingConsumer;
import com.rabbitmq.client.AMQP.BasicProperties;
import java.util.UUID;

public class RPCClient {
  
  private static final String RPC_QUEUE_NAME = "rpc_queue";
  
  static class FibonacciClient {
    private Channel channel;
    private String queue;
    
    public FibonacciClient(Channel chann, String queueName){
          channel = chann;
          queue = queueName;
    }
    
    public String call(String message)
                        throws java.io.IOException,
                               java.lang.InterruptedException {
      String replyQueueName = channel.queueDeclare().getQueue();      
      String corrId = UUID.randomUUID().toString();
      BasicProperties props = new BasicProperties();
      props.setReplyTo(replyQueueName);
      props.setCorrelationId(corrId);
      
      QueueingConsumer consumer = new QueueingConsumer(channel);
      channel.basicConsume(replyQueueName, true, consumer);

      channel.basicPublish("", queue, props, message.getBytes());
      
      String response = "";

      while (!validResponse(response)){
      	 QueueingConsumer.Delivery delivery = consumer.nextDelivery();
      	 response = new String(delivery.getBody());        	         	         
      }
      return response;  
    }
    
    private Boolean validResponse(String response){
    	    return (response.length() > 0);
    }
  }
  
  private static Connection defaultConnection()throws java.io.IOException{
    ConnectionFactory factory = new ConnectionFactory();
    factory.setHost("localhost");
    return factory.newConnection();
  }
  
  public static void main(String[] argv) 
                    throws java.io.IOException,
                           java.lang.InterruptedException {

    Connection connection = defaultConnection();
    Channel channel = connection.createChannel();
      
    RPCClient.FibonacciClient rpc = new RPCClient.FibonacciClient(channel, RPC_QUEUE_NAME);
    
    System.out.println(" [x] Requesting fib(30)");    
    String response = rpc.call("30"); 
    
    System.out.println(" [.] Got '" + response + "'");
    
    channel.close();
    connection.close();
  }  
}

