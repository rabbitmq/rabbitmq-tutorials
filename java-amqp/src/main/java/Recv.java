import com.rabbitmq.client.amqp.Connection;
import com.rabbitmq.client.amqp.Consumer;
import com.rabbitmq.client.amqp.Environment;
import com.rabbitmq.client.amqp.Management;

import java.nio.charset.StandardCharsets;

public class Recv {

    private static final String QUEUE_NAME = "hello";

    public static void main(String[] argv) throws Exception {
        Environment environment = TutorialSupport.newEnvironment();
        Connection connection = environment.connectionBuilder().uri(TutorialSupport.BROKER_URI).build();
        try (Management management = connection.management()) {
            management.queue().name(QUEUE_NAME).quorum().queue().declare();
        }

        System.out.println(" [*] Waiting for messages. To exit press CTRL+C");

        Consumer consumer = connection.consumerBuilder()
                .queue(QUEUE_NAME)
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
