import com.rabbitmq.client.BuiltinExchangeType;

public class EmitLogDirect {

    private static final String EXCHANGE_NAME = "direct_logs";

    public static void main(String[] argv) throws Exception {
        String severity = getSeverity(argv);
        String message = getMessage(argv);
        RabbitMQPublisher.publish(EXCHANGE_NAME, BuiltinExchangeType.DIRECT, severity, null, message);
    }

    private static String getSeverity(String[] strings) {
        if (strings.length < 1)
            return "info";
        return strings[0];
    }

    private static String getMessage(String[] strings) {
        if (strings.length < 2)
            return "Hello World!";
        return StringUtils.joinStrings(strings, " ", 1);
    }
}
