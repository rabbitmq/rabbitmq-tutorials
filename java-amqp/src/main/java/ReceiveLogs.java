import com.rabbitmq.client.amqp.Connection;
import com.rabbitmq.client.amqp.Consumer;
import com.rabbitmq.client.amqp.Environment;
import com.rabbitmq.client.amqp.Management;
import com.rabbitmq.client.amqp.Management.QueueInfo;

import java.nio.charset.StandardCharsets;

public class ReceiveLogs {

    private static final String EXCHANGE_NAME = "logs";

    public static void main(String[] argv) throws Exception {
        Environment environment = TutorialSupport.newEnvironment();
        Connection connection = environment.connectionBuilder().uri(TutorialSupport.BROKER_URI).build();

        String queueName;
        try (Management management = connection.management()) {
            management.exchange().name(EXCHANGE_NAME).type(Management.ExchangeType.FANOUT).declare();
            QueueInfo q = management.queue().exclusive(true).autoDelete(true).declare();
            queueName = q.name();
            management.binding()
                    .sourceExchange(EXCHANGE_NAME)
                    .destinationQueue(queueName)
                    .key("")
                    .bind();
        }

        System.out.println(" [*] Waiting for messages. To exit press CTRL+C");

        Consumer consumer = connection.consumerBuilder()
                .queue(queueName)
                .messageHandler((ctx, message) -> {
                    String body = new String(message.body(), StandardCharsets.UTF_8);
                    System.out.println(" [x] Received '" + body + "'");
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
}
