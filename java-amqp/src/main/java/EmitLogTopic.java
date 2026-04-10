import com.rabbitmq.client.amqp.Connection;
import com.rabbitmq.client.amqp.Environment;
import com.rabbitmq.client.amqp.Management;
import com.rabbitmq.client.amqp.Message;
import com.rabbitmq.client.amqp.Publisher;

import java.nio.charset.StandardCharsets;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

public class EmitLogTopic {

    private static final String EXCHANGE_NAME = "topic_logs";

    public static void main(String[] argv) throws Exception {
        Environment environment = TutorialSupport.newEnvironment();
        try (Connection connection = environment.connectionBuilder().uri(TutorialSupport.BROKER_URI).build()) {
            try (Management management = connection.management()) {
                management.exchange().name(EXCHANGE_NAME).type(Management.ExchangeType.TOPIC).declare();
            }
            String routingKey = getRouting(argv);
            try (Publisher publisher = connection.publisherBuilder().exchange(EXCHANGE_NAME).key(routingKey).build()) {
                String message = getMessage(argv);
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
                System.out.println(" [x] Sent '" + routingKey + "':'" + message + "'");
            }
        } finally {
            environment.close();
        }
    }

    private static String getRouting(String[] strings) {
        if (strings.length < 1) {
            return "anonymous.info";
        }
        return strings[0];
    }

    private static String getMessage(String[] strings) {
        if (strings.length < 2) {
            return "Hello World!";
        }
        return joinStrings(strings, " ", 1);
    }

    private static String joinStrings(String[] strings, String delimiter, int startIndex) {
        int length = strings.length;
        if (length == 0) {
            return "";
        }
        if (length < startIndex) {
            return "";
        }
        StringBuilder words = new StringBuilder(strings[startIndex]);
        for (int i = startIndex + 1; i < length; i++) {
            words.append(delimiter).append(strings[i]);
        }
        return words.toString();
    }
}
