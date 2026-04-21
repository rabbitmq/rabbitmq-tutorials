import com.rabbitmq.client.amqp.Connection;
import com.rabbitmq.client.amqp.Environment;
import com.rabbitmq.client.amqp.Management;
import com.rabbitmq.client.amqp.Message;
import com.rabbitmq.client.amqp.Publisher;

import java.time.Duration;
import java.util.UUID;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * AMQP 1.0 clients publish with broker feedback in the {@link Publisher.Callback} (accepted / rejected / released).
 * This is the counterpart to AMQP 0.9.1 publisher confirms.
 */
public class PublisherConfirms {

    static final int MESSAGE_COUNT = 50_000;

    public static void main(String[] args) throws Exception {
        publishMessagesIndividually();
        publishMessagesInBatch();
        publishMessagesWithOutstandingTracking();
    }

    static void publishMessagesIndividually() throws Exception {
        Environment environment = TutorialSupport.newEnvironment();
        try (Connection connection = environment.connectionBuilder().uri(TutorialSupport.BROKER_URI).build()) {
            String queue = UUID.randomUUID().toString();
            try (Management management = connection.management()) {
                management.queue().name(queue).exclusive(true).autoDelete(true).declare();
            }
            try (Publisher publisher = connection.publisherBuilder().queue(queue).build()) {
                long start = System.nanoTime();
                for (int i = 0; i < MESSAGE_COUNT; i++) {
                    String body = String.valueOf(i);
                    Message msg = publisher.message(body.getBytes());
                    CountDownLatch latch = new CountDownLatch(1);
                    publisher.publish(msg, ctx -> {
                        if (ctx.status() == Publisher.Status.ACCEPTED) {
                            latch.countDown();
                        }
                    });
                    if (!latch.await(30, TimeUnit.SECONDS)) {
                        throw new IllegalStateException("Confirm not received for message " + i);
                    }
                }
                long end = System.nanoTime();
                System.out.format("Published %,d messages individually in %,d ms%n", MESSAGE_COUNT,
                        Duration.ofNanos(end - start).toMillis());
            }
        } finally {
            environment.close();
        }
    }

    static void publishMessagesInBatch() throws Exception {
        Environment environment = TutorialSupport.newEnvironment();
        try (Connection connection = environment.connectionBuilder().uri(TutorialSupport.BROKER_URI).build()) {
            String queue = UUID.randomUUID().toString();
            try (Management management = connection.management()) {
                management.queue().name(queue).exclusive(true).autoDelete(true).declare();
            }
            try (Publisher publisher = connection.publisherBuilder().queue(queue).build()) {
                int batchSize = 100;
                long start = System.nanoTime();
                int i = 0;
                while (i < MESSAGE_COUNT) {
                    int n = Math.min(batchSize, MESSAGE_COUNT - i);
                    CountDownLatch batchLatch = new CountDownLatch(n);
                    for (int j = 0; j < n; j++) {
                        String body = String.valueOf(i + j);
                        Message msg = publisher.message(body.getBytes());
                        publisher.publish(msg, ctx -> {
                            if (ctx.status() == Publisher.Status.ACCEPTED) {
                                batchLatch.countDown();
                            }
                        });
                    }
                    if (!batchLatch.await(60, TimeUnit.SECONDS)) {
                        throw new IllegalStateException("Batch confirm incomplete at offset " + i);
                    }
                    i += n;
                }
                long end = System.nanoTime();
                System.out.format("Published %,d messages in batch in %,d ms%n", MESSAGE_COUNT,
                        Duration.ofNanos(end - start).toMillis());
            }
        } finally {
            environment.close();
        }
    }

    static void publishMessagesWithOutstandingTracking() throws Exception {
        Environment environment = TutorialSupport.newEnvironment();
        try (Connection connection = environment.connectionBuilder().uri(TutorialSupport.BROKER_URI).build()) {
            String queue = UUID.randomUUID().toString();
            try (Management management = connection.management()) {
                management.queue().name(queue).exclusive(true).autoDelete(true).declare();
            }
            final int maxOutstanding = 1000;
            try (Publisher publisher = connection.publisherBuilder().queue(queue).build()) {
                AtomicInteger outstanding = new AtomicInteger(0);
                CountDownLatch allDone = new CountDownLatch(MESSAGE_COUNT);
                long start = System.nanoTime();
                for (int i = 0; i < MESSAGE_COUNT; i++) {
                    while (outstanding.get() >= maxOutstanding) {
                        Thread.sleep(1);
                    }
                    outstanding.incrementAndGet();
                    String body = String.valueOf(i);
                    Message msg = publisher.message(body.getBytes());
                    publisher.publish(msg, ctx -> {
                        if (ctx.status() == Publisher.Status.ACCEPTED) {
                            outstanding.decrementAndGet();
                            allDone.countDown();
                        }
                    });
                }
                if (!allDone.await(120, TimeUnit.SECONDS)) {
                    throw new IllegalStateException("Not all messages confirmed");
                }
                long end = System.nanoTime();
                System.out.format("Published %,d messages with outstanding window in %,d ms%n", MESSAGE_COUNT,
                        Duration.ofNanos(end - start).toMillis());
            }
        } finally {
            environment.close();
        }
    }
}
