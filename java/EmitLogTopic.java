import com.rabbitmq.client.BuiltinExchangeType;

public class EmitLogTopic {

    private static final String EXCHANGE_NAME = "topic_logs";

    public static void main(String[] argv) throws Exception {
        String routingKey = getRouting(argv);
        String message = getMessage(argv);
        RabbitMQPublisher.publish(EXCHANGE_NAME, BuiltinExchangeType.TOPIC, routingKey, null, message);
    }

    private static String getRouting(String[] strings) {
        if (strings.length < 1)
            return "anonymous.info";
        return strings[0];
    }

    private static String getMessage(String[] strings) {
        if (strings.length < 2)
            return "Hello World!";
        return StringUtils.joinStrings(strings, " ", 1);
    }
}
