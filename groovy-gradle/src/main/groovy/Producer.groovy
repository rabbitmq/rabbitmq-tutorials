import com.rabbitmq.client.Channel
import com.rabbitmq.client.Connection
import com.rabbitmq.client.ConnectionFactory

import java.nio.charset.StandardCharsets

class Producer {

    private final static String QUEUE_NAME = "hello";

    static void main(String[] args) {

        def factory = new ConnectionFactory();
        factory.setHost("localhost");
        try (def connection = factory.newConnection();
             def channel = connection.createChannel()) {
            channel.queueDeclare(QUEUE_NAME, false, false, false, null);
            def message = "Hello World!";
            channel.basicPublish("", QUEUE_NAME, null, message.getBytes(StandardCharsets.UTF_8));
            System.out.println(" [x] Sent '" + message + "'");
        }
    }
}
