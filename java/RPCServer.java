import java.io.IOException;
import com.rabbitmq.client.ConnectionFactory;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.Channel;
import com.rabbitmq.client.QueueingConsumer;
import com.rabbitmq.client.AMQP.BasicProperties;
  
public class RPCServer {

  private static final String RPC_QUEUE_NAME = "rpc_queue";

  public static void main(String[] argv)
                         throws java.io.IOException,
                                java.lang.InterruptedException {

    ConnectionFactory factory = new ConnectionFactory();
    factory.setHost("localhost");
    Connection connection = factory.newConnection();
    Channel channel = connection.createChannel();
        
    channel.queueDeclare(RPC_QUEUE_NAME, false, false, false, null);
   
    channel.basicQos(1);
    
    QueueingConsumer consumer = new QueueingConsumer(channel);
    channel.basicConsume(RPC_QUEUE_NAME, false, consumer);
    
    System.out.println(" [x] Awaiting RPC requests");
     
    while (true) {
      QueueingConsumer.Delivery delivery = consumer.nextDelivery();
      String message = new String(delivery.getBody());
      int n = Integer.parseInt(message);
      
      System.out.println(" [.] fib(" + message + ")");
      String response = "" + fib(n);
      
      BasicProperties props = delivery.getProperties();

      channel.basicPublish( "", props.getReplyTo(), props, response.getBytes());

      channel.basicAck(delivery.getEnvelope().getDeliveryTag(), false);
    }         
  }
  
  private static int fib(int n) throws InterruptedException {
    if (n == 0)
    	    return 0;	
    else if (n == 1)
    	    return 1;
    else return fib(n-1) + fib(n-2);
    
  }
}

