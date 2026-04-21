import com.rabbitmq.client.amqp.Connection;
import com.rabbitmq.client.amqp.Environment;
import com.rabbitmq.client.amqp.Management;
import com.rabbitmq.client.amqp.Message;
import com.rabbitmq.client.amqp.Publisher;

import java.nio.charset.StandardCharsets;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

public class EmitLog {

    private static final String EXCHANGE_NAME = "logs";

    public static void main(String[] argv) throws Exception {
        Environment environment = TutorialSupport.newEnvironment();
        try (Connection connection = environment.connectionBuilder().uri(TutorialSupport.BROKER_URI).build()) {
            try (Management management = connection.management()) {
                management.exchange().name(EXCHANGE_NAME).type(Management.ExchangeType.FANOUT).declare();
            }
            try (Publisher publisher = connection.publisherBuilder().exchange(EXCHANGE_NAME).build()) {
                String message = argv.length < 1 ? "info: Hello World!" : String.join(" ", argv);
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
