import com.rabbitmq.client.Channel;
import com.rabbitmq.client.ConfirmCallback;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.ConnectionFactory;

import java.time.Duration;
import java.util.LinkedList;
import java.util.UUID;
import java.util.concurrent.ConcurrentNavigableMap;
import java.util.concurrent.ConcurrentSkipListMap;
import java.util.function.BooleanSupplier;

public class PublisherConfirms {

    static final int MESSAGE_COUNT = 50_000;

    static Connection createConnection() throws Exception {
        ConnectionFactory cf = new ConnectionFactory();
        cf.setHost("localhost");
        cf.setUsername("guest");
        cf.setPassword("guest");
        return cf.newConnection();
    }

    static final int MAX_OUTSTANDING = 1000; // Confirmation window
    static final int THROTTLING_PERCENTAGE = 50; // Start throttling at 50% capacity
    static final int MAX_DELAY_MS = 1000; // Maximum delay in milliseconds

    public static void main(String[] args) throws Exception {
        publishMessagesIndividually();
        publishMessagesInBatch();
        handlePublishConfirmsAsynchronously();
        handlePublishConfirmsWithWindow();
        handlePublishConfirmsWithAdaptiveThrottling();
    }

    static void publishMessagesIndividually() throws Exception {
        try (Connection connection = createConnection()) {
            Channel ch = connection.createChannel();

            String queue = UUID.randomUUID().toString();
            ch.queueDeclare(queue, false, false, true, null);

            ch.confirmSelect();
            long start = System.nanoTime();
            for (int i = 0; i < MESSAGE_COUNT; i++) {
                String body = String.valueOf(i);
                ch.basicPublish("", queue, null, body.getBytes());
                ch.waitForConfirmsOrDie(5_000);
            }
            long end = System.nanoTime();
            System.out.format("Published %,d messages individually in %,d ms%n", MESSAGE_COUNT, Duration.ofNanos(end - start).toMillis());
        }
    }

    static void publishMessagesInBatch() throws Exception {
        try (Connection connection = createConnection()) {
            Channel ch = connection.createChannel();

            String queue = UUID.randomUUID().toString();
            ch.queueDeclare(queue, false, false, true, null);

            ch.confirmSelect();

            int batchSize = 100;
            int outstandingMessageCount = 0;

            long start = System.nanoTime();
            for (int i = 0; i < MESSAGE_COUNT; i++) {
                String body = String.valueOf(i);
                ch.basicPublish("", queue, null, body.getBytes());
                outstandingMessageCount++;

                if (outstandingMessageCount == batchSize) {
                    ch.waitForConfirmsOrDie(5_000);
                    outstandingMessageCount = 0;
                }
            }

            if (outstandingMessageCount > 0) {
                ch.waitForConfirmsOrDie(5_000);
            }
            long end = System.nanoTime();
            System.out.format("Published %,d messages in batch in %,d ms%n", MESSAGE_COUNT, Duration.ofNanos(end - start).toMillis());
        }
    }

    static void handlePublishConfirmsAsynchronously() throws Exception {
        try (Connection connection = createConnection()) {
            Channel ch = connection.createChannel();

            String queue = UUID.randomUUID().toString();
            ch.queueDeclare(queue, false, false, true, null);

            ch.confirmSelect();

            ConcurrentNavigableMap<Long, String> outstandingConfirms = new ConcurrentSkipListMap<>();

            ConfirmCallback cleanOutstandingConfirms = (sequenceNumber, multiple) -> {
                if (multiple) {
                    ConcurrentNavigableMap<Long, String> confirmed = outstandingConfirms.headMap(
                            sequenceNumber, true
                    );
                    confirmed.clear();
                } else {
                    outstandingConfirms.remove(sequenceNumber);
                }
            };

            ch.addConfirmListener(cleanOutstandingConfirms, (sequenceNumber, multiple) -> {
                String body = outstandingConfirms.get(sequenceNumber);
                System.err.format(
                        "Message with body %s has been nack-ed. Sequence number: %d, multiple: %b%n",
                        body, sequenceNumber, multiple
                );
                cleanOutstandingConfirms.handle(sequenceNumber, multiple);
            });

            long start = System.nanoTime();
            for (int i = 0; i < MESSAGE_COUNT; i++) {
                String body = String.valueOf(i);
                outstandingConfirms.put(ch.getNextPublishSeqNo(), body);
                ch.basicPublish("", queue, null, body.getBytes());
            }

            if (!waitUntil(Duration.ofSeconds(60), () -> outstandingConfirms.isEmpty())) {
                throw new IllegalStateException("All messages could not be confirmed in 60 seconds");
            }

            long end = System.nanoTime();
            System.out.format("Published %,d messages and handled confirms asynchronously in %,d ms%n", MESSAGE_COUNT, Duration.ofNanos(end - start).toMillis());
        }
    }

    static void handlePublishConfirmsWithWindow() throws Exception {
        try (Connection connection = createConnection()) {
            Channel ch = connection.createChannel();

            String queue = UUID.randomUUID().toString();
            ch.queueDeclare(queue, false, false, true, null);
            ch.confirmSelect();

            ConcurrentNavigableMap<Long, String> outstandingConfirms = new ConcurrentSkipListMap<>();

            ConfirmCallback cleanOutstandingConfirms = (sequenceNumber, multiple) -> {
                if (multiple) {
                    outstandingConfirms.headMap(sequenceNumber, true).clear();
                } else {
                    outstandingConfirms.remove(sequenceNumber);
                }
                synchronized (outstandingConfirms) {
                    outstandingConfirms.notifyAll();
                }
            };

            ch.addConfirmListener(cleanOutstandingConfirms, (sequenceNumber, multiple) -> {
                System.err.format("Message nacked. Sequence: %d, multiple: %b%n", sequenceNumber, multiple);
                cleanOutstandingConfirms.handle(sequenceNumber, multiple);
            });

            long start = System.nanoTime();
            for (int i = 0; i < MESSAGE_COUNT; i++) {
                // Wait if window is full
                synchronized (outstandingConfirms) {
                    while (outstandingConfirms.size() >= MAX_OUTSTANDING) {
                        outstandingConfirms.wait();
                    }
                }

                String body = String.valueOf(i);
                outstandingConfirms.put(ch.getNextPublishSeqNo(), body);
                ch.basicPublish("", queue, null, body.getBytes());
            }

            // Wait for remaining confirmations
            synchronized (outstandingConfirms) {
                while (!outstandingConfirms.isEmpty()) {
                    outstandingConfirms.wait();
                }
            }

            long end = System.nanoTime();
            System.out.format("Published %,d messages with confirmation window in %,d ms%n",
                MESSAGE_COUNT, Duration.ofNanos(end - start).toMillis());
        }
    }

    static void handlePublishConfirmsWithAdaptiveThrottling() throws Exception {
        try (Connection connection = createConnection()) {
            Channel ch = connection.createChannel();

            String queue = UUID.randomUUID().toString();
            ch.queueDeclare(queue, false, false, true, null);
            ch.confirmSelect();

            LinkedList<Long> outstandingConfirms = new LinkedList<>();
            int throttlingThreshold = MAX_OUTSTANDING * THROTTLING_PERCENTAGE / 100;

            ConfirmCallback cleanOutstandingConfirms = (sequenceNumber, multiple) -> {
                synchronized (outstandingConfirms) {
                    if (multiple) {
                        outstandingConfirms.removeIf(seqNo -> seqNo <= sequenceNumber);
                    } else {
                        outstandingConfirms.removeFirstOccurrence(sequenceNumber);
                    }
                    outstandingConfirms.notifyAll();
                }
            };

            ch.addConfirmListener(cleanOutstandingConfirms, (sequenceNumber, multiple) -> {
                System.err.format("Message nacked. Sequence: %d, multiple: %b%n", sequenceNumber, multiple);
                cleanOutstandingConfirms.handle(sequenceNumber, multiple);
            });

            long start = System.nanoTime();
            for (int i = 0; i < MESSAGE_COUNT; i++) {
                String body = String.valueOf(i);

                synchronized (outstandingConfirms) {
                    while (outstandingConfirms.size() >= MAX_OUTSTANDING) {
                        outstandingConfirms.wait();
                    }

                    int availablePermits = MAX_OUTSTANDING - outstandingConfirms.size();
                    if (availablePermits < throttlingThreshold) {
                        double percentageUsed = 1.0 - (availablePermits / (double) MAX_OUTSTANDING);
                        int delay = (int) (percentageUsed * MAX_DELAY_MS);
                        if (delay > 0) {
                            outstandingConfirms.wait(delay);
                        }
                    }

                    long seqNo = ch.getNextPublishSeqNo();
                    outstandingConfirms.addLast(seqNo);
                }

                ch.basicPublish("", queue, null, body.getBytes());
            }

            synchronized (outstandingConfirms) {
                while (!outstandingConfirms.isEmpty()) {
                    outstandingConfirms.wait();
                }
            }

            long end = System.nanoTime();
            System.out.format("Published %,d messages with adaptive throttling in %,d ms%n",
                MESSAGE_COUNT, Duration.ofNanos(end - start).toMillis());
        }
    }

    static boolean waitUntil(Duration timeout, BooleanSupplier condition) throws InterruptedException {
        int waited = 0;
        while (!condition.getAsBoolean() && waited < timeout.toMillis()) {
            Thread.sleep(100L);
            waited += 100;
        }
        return condition.getAsBoolean();
    }

}
