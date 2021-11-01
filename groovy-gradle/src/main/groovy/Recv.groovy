import com.rabbitmq.client.Channel
import com.rabbitmq.client.Connection
import com.rabbitmq.client.ConnectionFactory
import com.rabbitmq.client.ConsumerShutdownSignalCallback
import com.rabbitmq.client.DeliverCallback

import java.nio.charset.StandardCharsets

class Recv {

    private final static String QUEUE_NAME = "hello";

    static void main(String[] argv) throws Exception {
        def factory = new ConnectionFactory();
        factory.setHost("localhost");
        def connection = factory.newConnection();
        def channel = connection.createChannel();

        channel.queueDeclare(QUEUE_NAME, false, false, false, null);
        System.out.println(" [*] Waiting for messages. To exit press CTRL+C");

        DeliverCallback deliverCallback = (consumerTag, delivery) -> {
            String message = new String(delivery.getBody(), StandardCharsets.UTF_8);
            System.out.println(" [x] Received '" + message + "'");
        };
        ConsumerShutdownSignalCallback callback = { }
        channel.basicConsume(QUEUE_NAME, true, deliverCallback, callback)
    }
}
