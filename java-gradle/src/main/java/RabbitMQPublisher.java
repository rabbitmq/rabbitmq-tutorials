import com.rabbitmq.client.AMQP;
import com.rabbitmq.client.BuiltinExchangeType;
import com.rabbitmq.client.Channel;
import com.rabbitmq.client.Connection;

import java.nio.charset.StandardCharsets;

public class RabbitMQPublisher {

    public static void publish(String exchangeName, BuiltinExchangeType exchangeType, String routingKey, AMQP.BasicProperties props, String message) throws Exception {
        try (Connection connection = ConnectionManager.createConnection();
             Channel channel = connection.createChannel()) {
            channel.exchangeDeclare(exchangeName, exchangeType);
            channel.basicPublish(exchangeName, routingKey, props, message.getBytes(StandardCharsets.UTF_8));
            System.out.println(" [x] Sent '" + (routingKey != null && !routingKey.isEmpty() ? routingKey + "':'" : "") + message + "'");
        }
    }
}
