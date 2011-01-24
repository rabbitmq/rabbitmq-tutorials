import com.rabbitmq.client.ConnectionFactory;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.Channel;
import com.rabbitmq.client.QueueingConsumer;
  
public class Worker {
  public static void main(String[] argv)
                         throws java.io.IOException,
                                java.lang.InterruptedException {
    Connection connection = null;
    ConnectionFactory factory = new ConnectionFactory();
    factory.setHost("localhost");
    connection = factory.newConnection();
    Channel channel = connection.createChannel();
    
    channel.queueDeclare("task_queue", true, false, false, null);
    System.out.println(" [*] Waiting for messages. To exit press CTRL+C");
    
    channel.basicQos(1);
    
    QueueingConsumer consumer = new QueueingConsumer(channel);
    channel.basicConsume("task_queue", false, consumer);
    
    while (true) {
      QueueingConsumer.Delivery delivery = consumer.nextDelivery();
      String body = new String(delivery.getBody());
      System.out.println(" [x] Received " + body);

      Thread.sleep( charCount(body, '.') * 1000); 
      System.out.println(" [x] Done" );

      channel.basicAck(delivery.getEnvelope().getDeliveryTag(), false);
    }
  }
  
  private static int charCount(String body, char theChar){ 
    int count = 0;    
    for (int index = 0; index < body.length(); index++){
      if (theChar == body.charAt(index)) count++;
    }
    return count;
  }
}

