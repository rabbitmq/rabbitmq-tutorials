import com.rabbitmq.client.*;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class ReceiveLogHeader {
  private static final String EXCHANGE_NAME = "header_test";

  public static void main(String[] argv) throws Exception {
    if (argv.length < 1) {
      System.err.println("Usage: ReceiveLogsHeader queueName [headers]...");
      System.exit(1);
    }

    ConnectionFactory factory = new ConnectionFactory();
    factory.setHost("localhost");
    Connection connection = factory.newConnection();
    Channel channel = connection.createChannel();

    channel.exchangeDeclare(EXCHANGE_NAME, "headers");

    // The API requires a routing key, but in fact if you are using a header exchange the
    // value of the routing key is not used in the routing. You can receive information
    // from the sender here as the routing key is still available in the received message.
    String routingKeyFromUser = "ourTestRoutingKey";

    // Argument processing: the first arg is the local queue name, the rest are
    // key value pairs for headers.
    String queueInputName = argv[0];

    // The map for the headers.
    Map<String, Object> headers = new HashMap<String, Object>();

    // The rest of the arguments are key value header pairs.  For the purpose of this
    // example, we are assuming they are all strings, but that is not required by RabbitMQ
    // Note that when you run this code you should include the x-match header on the command
    // line. Example:
    //    java -cp $CP ReceiveLogsHeader testQueue1  x-match any header1 value1
    for (int i = 1; i < argv.length; i++) {
      headers.put(argv[i], argv[i + 1]);
      System.out.println("Binding header " + argv[i] + " and value " + argv[i + 1] + " to queue " + queueInputName);
      i++;
    }

    String queueName = channel.queueDeclare(queueInputName, true, false, false, null).getQueue();
    channel.queueBind(queueName, EXCHANGE_NAME, routingKeyFromUser, headers);

    System.out.println(" [*] Waiting for messages. To exit press CTRL+C");

    Consumer consumer = new DefaultConsumer(channel) {
      @Override
      public void handleDelivery(String consumerTag, Envelope envelope,
                                 AMQP.BasicProperties properties, byte[] body) throws IOException {
        String message = new String(body, "UTF-8");
        System.out.println(" [x] Received '" + envelope.getRoutingKey() + "':'" + message + "'");
      }
    };
    channel.basicConsume(queueName, true, consumer);
  }
}

