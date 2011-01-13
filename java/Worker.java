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

        boolean durable = true;
        channel.queueDeclare("task_queue", durable, false, false, null);
        System.out.println(" [*] Waiting for messages. To exit press CTRL+C");

        int prefetchCount = 1;
        channel.basicQos(prefetchCount);

        QueueingConsumer consumer = new QueueingConsumer(channel);
        boolean autoAck = false;
        channel.basicConsume("task_queue", autoAck, consumer);

        while (true) {
            QueueingConsumer.Delivery delivery = consumer.nextDelivery();
            String body = new String(delivery.getBody());
            System.out.println(" [x] Received " + body);
            Thread.sleep(doDots(body)); // simulate action
            System.out.println(" [x] Done");
            // acknowledge
            channel.basicAck(delivery.getEnvelope().getDeliveryTag(), false);
        }
    }



  private static int doDots(String body){ 
// just do something to get a number for simulation
	int x = body.indexOf('.') ;
	if (x < 0) return 0;
                body = body.substring(x); 
	return body.length() ;
  }
}
