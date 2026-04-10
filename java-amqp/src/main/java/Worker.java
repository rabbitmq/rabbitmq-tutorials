import com.rabbitmq.client.amqp.Connection;
import com.rabbitmq.client.amqp.Consumer;
import com.rabbitmq.client.amqp.Environment;
import com.rabbitmq.client.amqp.Management;

import java.nio.charset.StandardCharsets;

public class Worker {

    private static final String TASK_QUEUE_NAME = "task_queue";

    public static void main(String[] argv) throws Exception {
        Environment environment = TutorialSupport.newEnvironment();
        Connection connection = environment.connectionBuilder().uri(TutorialSupport.BROKER_URI).build();
        try (Management management = connection.management()) {
            management.queue().name(TASK_QUEUE_NAME).quorum().queue().declare();
        }

        System.out.println(" [*] Waiting for messages. To exit press CTRL+C");

        Consumer consumer = connection.consumerBuilder()
                .queue(TASK_QUEUE_NAME)
                .initialCredits(1)
                .messageHandler((ctx, message) -> {
                    String body = new String(message.body(), StandardCharsets.UTF_8);
                    System.out.println(" [x] Received '" + body + "'");
                    try {
                        doWork(body);
                    } finally {
                        System.out.println(" [x] Done");
                        ctx.accept();
                    }
                })
                .build();

        Runtime.getRuntime().addShutdownHook(new Thread(() -> {
            consumer.close();
            connection.close();
            environment.close();
        }));

        Thread.sleep(Long.MAX_VALUE);
    }

    private static void doWork(String task) {
        for (char ch : task.toCharArray()) {
            if (ch == '.') {
                try {
                    Thread.sleep(1000);
                } catch (InterruptedException ignored) {
                    Thread.currentThread().interrupt();
                }
            }
        }
    }
}
