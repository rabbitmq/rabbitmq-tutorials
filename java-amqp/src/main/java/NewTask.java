import com.rabbitmq.client.amqp.Connection;
import com.rabbitmq.client.amqp.Environment;
import com.rabbitmq.client.amqp.Management;
import com.rabbitmq.client.amqp.Message;
import com.rabbitmq.client.amqp.Publisher;

import java.nio.charset.StandardCharsets;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

public class NewTask {

    private static final String TASK_QUEUE_NAME = "task_queue";

    public static void main(String[] argv) throws Exception {
        Environment environment = TutorialSupport.newEnvironment();
        try (Connection connection = environment.connectionBuilder().uri(TutorialSupport.BROKER_URI).build()) {
            try (Management management = connection.management()) {
                management.queue(TASK_QUEUE_NAME).quorum().queue().declare();
                // Un-comment the following line to declare a Classic Queue
                // Comment the previous queue declaration, if you un-comment below
                // management.queue(TASK_QUEUE_NAME).classic().queue().declare();
            }
            try (Publisher publisher = connection.publisherBuilder().queue(TASK_QUEUE_NAME).build()) {
                String message = String.join(" ", argv);
                Message msg = publisher.message(message.getBytes(StandardCharsets.UTF_8)).durable(true);
                CountDownLatch confirmed = new CountDownLatch(1);
                publisher.publish(msg, ctx -> {
                    if (ctx.status() == Publisher.Status.ACCEPTED) {
                        confirmed.countDown();
                    }
                });
                if (!confirmed.await(30, TimeUnit.SECONDS)) {
                    throw new IllegalStateException("Publish was not confirmed in time");
                }
                System.out.println(" [x] Sent '" + message + "'");
            }
        } finally {
            environment.close();
        }
    }
}
