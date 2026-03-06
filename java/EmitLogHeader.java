import com.rabbitmq.client.AMQP;
import com.rabbitmq.client.BuiltinExchangeType;
import com.rabbitmq.client.MessageProperties;

import java.util.HashMap;
import java.util.Map;

public class EmitLogHeader {

    private static final String EXCHANGE_NAME = "header_test";

    public static void main(String[] argv) throws Exception {
        if (argv.length < 1) {
            System.err.println("Usage: EmitLogHeader message queueName [headers]...");
            System.exit(1);
        }

        String routingKey = "ourTestRoutingKey";
        String message = argv[0];

        Map<String, Object> headers = new HashMap<>();
        for (int i = 1; i < argv.length; i++) {
            headers.put(argv[i], argv[i + 1]);
            i++;
        }

        AMQP.BasicProperties.Builder builder = new AMQP.BasicProperties.Builder();
        builder.deliveryMode(MessageProperties.PERSISTENT_TEXT_PLAIN.getDeliveryMode());
        builder.priority(MessageProperties.PERSISTENT_TEXT_PLAIN.getPriority());
        builder.headers(headers);
        AMQP.BasicProperties theProps = builder.build();

        RabbitMQPublisher.publish(EXCHANGE_NAME, BuiltinExchangeType.HEADERS, routingKey, theProps, message);
    }
}
