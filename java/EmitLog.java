import com.rabbitmq.client.BuiltinExchangeType;

public class EmitLog {

    private static final String EXCHANGE_NAME = "logs";

    public static void main(String[] argv) throws Exception {
        String message = argv.length < 1 ? "info: Hello World!" : String.join(" ", argv);
        RabbitMQPublisher.publish(EXCHANGE_NAME, BuiltinExchangeType.FANOUT, "", null, message);
    }
}
