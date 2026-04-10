import com.rabbitmq.client.amqp.Connection;
import com.rabbitmq.client.amqp.Consumer;
import com.rabbitmq.client.amqp.Environment;
import com.rabbitmq.client.amqp.Management;
import com.rabbitmq.client.amqp.Management.QueueInfo;
import com.rabbitmq.client.amqp.Message;

import java.nio.charset.StandardCharsets;

public class ReceiveLogsDirect {

    private static final String EXCHANGE_NAME = "direct_logs";

    public static void main(String[] argv) throws Exception {
        if (argv.length < 1) {
            System.err.println("Usage: ReceiveLogsDirect [info] [warning] [error]");
            System.exit(1);
        }

        Environment environment = TutorialSupport.newEnvironment();
        Connection connection = environment.connectionBuilder().uri(TutorialSupport.BROKER_URI).build();

        String queueName;
        try (Management management = connection.management()) {
            management.exchange().name(EXCHANGE_NAME).type(Management.ExchangeType.DIRECT).declare();
            QueueInfo q = management.queue().exclusive(true).autoDelete(true).declare();
            queueName = q.name();
            for (String severity : argv) {
                management.binding()
                        .sourceExchange(EXCHANGE_NAME)
                        .destinationQueue(queueName)
                        .key(severity)
                        .bind();
            }
        }

        System.out.println(" [*] Waiting for messages. To exit press CTRL+C");

        Consumer consumer = connection.consumerBuilder()
                .queue(queueName)
                .messageHandler((ctx, message) -> {
                    String body = new String(message.body(), StandardCharsets.UTF_8);
                    String routingKey = routingKey(message);
                    System.out.println(" [x] Received '" + routingKey + "':'" + body + "'");
                    ctx.accept();
                })
                .build();

        Runtime.getRuntime().addShutdownHook(new Thread(() -> {
            consumer.close();
            connection.close();
            environment.close();
        }));

        Thread.sleep(Long.MAX_VALUE);
    }

    private static String routingKey(Message message) {
        Object rk = message.annotation("x-routing-key");
        if (rk != null) {
            return rk.toString();
        }
        String subject = message.subject();
        return subject != null ? subject : "";
    }
}
