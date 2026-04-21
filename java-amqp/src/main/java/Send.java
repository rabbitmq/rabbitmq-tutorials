import com.rabbitmq.client.amqp.Connection;
import com.rabbitmq.client.amqp.Environment;
import com.rabbitmq.client.amqp.Management;
import com.rabbitmq.client.amqp.Message;
import com.rabbitmq.client.amqp.Publisher;

import java.nio.charset.StandardCharsets;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

public class Send {

    private static final String QUEUE_NAME = "hello";

    public static void main(String[] argv) throws Exception {
        Environment environment = TutorialSupport.newEnvironment();
        try (Connection connection = environment.connectionBuilder().uri(TutorialSupport.BROKER_URI).build()) {
            try (Management management = connection.management()) {
                management.queue().name(QUEUE_NAME).quorum().queue().declare();
            }
            try (Publisher publisher = connection.publisherBuilder().queue(QUEUE_NAME).build()) {
                String message = "Hello World!";
                Message msg = publisher.message(message.getBytes(StandardCharsets.UTF_8));
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
