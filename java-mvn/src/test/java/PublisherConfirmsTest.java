import com.codahale.metrics.Meter;
import com.codahale.metrics.MetricRegistry;
import com.rabbitmq.client.Channel;
import com.rabbitmq.client.ConfirmCallback;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.ConnectionFactory;
import org.junit.jupiter.api.*;

import java.io.IOException;
import java.time.Duration;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.BooleanSupplier;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

/**
 * Tests to illustrate different ways to handle publisher confirms.
 */
public class PublisherConfirmsTest {

    private final MetricRegistry metrics = new MetricRegistry();
    private final Meter meter = metrics.meter("outbound-messages");
    Connection connection;
    String queue;
    int messageCount = 10_000;

    static boolean waitUntil(Duration timeout, BooleanSupplier condition) throws InterruptedException {
        int waited = 0;
        while (!condition.getAsBoolean() && waited < timeout.toMillis()) {
            Thread.sleep(100L);
            waited = +100;
        }
        return condition.getAsBoolean();
    }

    @BeforeEach
    void init() throws Exception {
        ConnectionFactory cf = new ConnectionFactory();
        cf.setHost("localhost");
        cf.setUsername("guest");
        cf.setPassword("guest");
        connection = cf.newConnection();
        queue = UUID.randomUUID().toString();
        try (Channel ch = connection.createChannel()) {
            ch.queueDeclare(queue, false, false, true, null);
        }
    }

    @AfterEach
    void tearDown(TestInfo testInfo) throws Exception {
        System.out.println(String.format("%s: %.0f messages/second", testInfo.getDisplayName(), meter.getMeanRate()));
        connection.close(10_000);
    }

    @Test
    @DisplayName("publish messages individually")
    void publishMessagesIndividually() throws Exception {
        Channel ch = connection.createChannel();
        ch.confirmSelect();
        for (int i = 0; i < messageCount; i++) {
            String body = String.valueOf(i);
            ch.basicPublish("", queue, null, body.getBytes());
            ch.waitForConfirmsOrDie(5_000);
            meter.mark();
        }
        ch.close();

        CountDownLatch latch = new CountDownLatch(messageCount);
        ch = connection.createChannel();
        ch.basicConsume(queue, true, ((consumerTag, message) -> latch.countDown()), consumerTag -> {
        });

        assertThat(latch.await(60, TimeUnit.SECONDS)).isTrue();
    }

    @Test
    @DisplayName("publish messages in batch")
    void publishMessagesInBatch() throws Exception {
        Channel ch = connection.createChannel();
        ch.confirmSelect();
        int batchSize = 100;
        int outstandingMessageCount = 0;
        for (int i = 0; i < messageCount; i++) {
            String body = String.valueOf(i);
            ch.basicPublish("", queue, null, body.getBytes());
            outstandingMessageCount++;

            if (outstandingMessageCount == batchSize) {
                ch.waitForConfirmsOrDie(5_000);
                outstandingMessageCount = 0;
            }
            meter.mark();
        }

        if (outstandingMessageCount > 0) {
            ch.waitForConfirmsOrDie(5_000);
        }

        ch.close();

        CountDownLatch latch = new CountDownLatch(messageCount);
        ch = connection.createChannel();
        ch.basicConsume(queue, true, ((consumerTag, message) -> latch.countDown()), consumerTag -> {
        });

        assertThat(latch.await(60, TimeUnit.SECONDS)).isTrue();
    }

    @Test
    @DisplayName("allow max of outstanding confirms")
    void allowMaxOfOutstandingConfirms() throws Exception {
        int maxOutstandingConfirms = 100;
        Semaphore semaphore = new Semaphore(maxOutstandingConfirms);
        Channel ch = connection.createChannel();
        ch.confirmSelect();
        ConcurrentSkipListSet<Long> outstandingConfirms = new ConcurrentSkipListSet<>();
        ConfirmCallback handleConfirm = (deliveryTag, multiple) -> {
            if (multiple) {
                NavigableSet<Long> confirmed = outstandingConfirms.headSet(deliveryTag, true);
                int confirmedSize = confirmed.size();
                confirmed.clear();
                semaphore.release(confirmedSize);
            } else {
                outstandingConfirms.remove(deliveryTag);
                semaphore.release();
            }
        };
        ch.addConfirmListener(handleConfirm, handleConfirm);
        for (int i = 0; i < messageCount; i++) {
            String body = String.valueOf(i);
            boolean acquired = semaphore.tryAcquire(10, TimeUnit.SECONDS);
            if (!acquired) {
                throw new IllegalStateException("Could not publish because of too many outstanding publisher confirms");
            }
            outstandingConfirms.add(ch.getNextPublishSeqNo());
            ch.basicPublish("", queue, null, body.getBytes());
            meter.mark();
        }
        assertThat(waitUntil(Duration.ofSeconds(5), () -> outstandingConfirms.isEmpty())).isTrue();
        ch.close();

        CountDownLatch latch = new CountDownLatch(messageCount);
        ch = connection.createChannel();
        ch.basicConsume(queue, true, ((consumerTag, message) -> latch.countDown()), consumerTag -> {
        });

        assertThat(latch.await(60, TimeUnit.SECONDS)).isTrue();
    }

    @Test
    @DisplayName("resend unconfirmed messages")
    void resendUnconfirmedMessagesIntegration() throws Exception {
        Channel ch = connection.createChannel();
        ch.confirmSelect();
        Map<Long, String> outstandingConfirms = resendUnconfirmedMessages(ch);
        assertThat(waitUntil(Duration.ofSeconds(5), () -> outstandingConfirms.isEmpty())).isTrue();
        ch.close();

        Set<String> receivedMessages = ConcurrentHashMap.newKeySet(messageCount);
        CountDownLatch latch = new CountDownLatch(messageCount);
        ch = connection.createChannel();
        ch.basicConsume(queue, true, ((consumerTag, message) -> {
            receivedMessages.add(new String(message.getBody()));
            latch.countDown();
        }), consumerTag -> {
        });

        assertThat(latch.await(60, TimeUnit.SECONDS)).isTrue();
        assertThat(receivedMessages).hasSize(messageCount);
    }

    @Test
    @DisplayName("resend unconfirmed messages (mock)")
    void resendUnconfirmedMessagesMock() throws Exception {
        Channel ch = mock(Channel.class);

        Set<String> receivedMessages = configureMockServer(ch);

        resendUnconfirmedMessages(ch);

        assertThat(receivedMessages).hasSize(messageCount);
    }

    @Test
    @DisplayName("resend unconfirmed messages with lower bound and map confirm handling")
    void resendUnconfirmedMessagesUseLowerBoundAndConcurrentMapIntegration() throws Exception {
        Channel ch = connection.createChannel();
        ch.confirmSelect();
        Map<Long, String> outstandingConfirms = resendUnconfirmedMessagesUseLowerBoundAndConcurrentMap(ch);
        assertThat(waitUntil(Duration.ofSeconds(5), () -> outstandingConfirms.isEmpty())).isTrue();
        ch.close();

        Set<String> receivedMessages = ConcurrentHashMap.newKeySet(messageCount);
        CountDownLatch latch = new CountDownLatch(messageCount);
        ch = connection.createChannel();
        ch.basicConsume(queue, true, ((consumerTag, message) -> {
            receivedMessages.add(new String(message.getBody()));
            latch.countDown();
        }), consumerTag -> {
        });

        assertThat(latch.await(60, TimeUnit.SECONDS)).isTrue();
        assertThat(receivedMessages).hasSize(messageCount);
    }

    @Test
    @DisplayName("resend unconfirmed messages with lower bound and map confirm handling (mock)")
    void resendUnconfirmedMessagesUseLowerBoundAndConcurrentMapMock() throws Exception {
        Channel ch = mock(Channel.class);

        Set<String> receivedMessages = configureMockServer(ch);

        resendUnconfirmedMessagesUseLowerBoundAndConcurrentMap(ch);

        assertThat(receivedMessages).hasSize(messageCount);
    }

    /**
     * Configure a mock channel that will publish to a server-like threads.
     * The fake server will send publisher confirms notification (ack, nack, multiple
     * or not) randomly.
     * Allows to test publisher confirms handling in a reliable way.
     */
    private Set<String> configureMockServer(Channel ch) throws IOException {
        AtomicLong clientPublishingSequence = new AtomicLong(0);
        when(ch.getNextPublishSeqNo()).thenAnswer(invocationOnMock -> clientPublishingSequence.getAndIncrement());
        BlockingQueue<String> messagesSentToServer = new LinkedBlockingQueue<>();
        doAnswer(invocation -> {
            messagesSentToServer.offer(new String(invocation.getArgument(3, byte[].class)));
            return null;
        }).when(ch).basicPublish(anyString(), anyString(), isNull(), any(byte[].class));
        AtomicReference<ConfirmCallback> handleAckReference = new AtomicReference<>();
        AtomicReference<ConfirmCallback> handleNackReference = new AtomicReference<>();
        doAnswer(invocation -> {
            handleAckReference.set(invocation.getArgument(0, ConfirmCallback.class));
            handleNackReference.set(invocation.getArgument(1, ConfirmCallback.class));
            return null;
        }).when(ch).addConfirmListener(any(ConfirmCallback.class), any(ConfirmCallback.class));

        Set<String> receivedMessages = ConcurrentHashMap.newKeySet(messageCount);
        new Thread(() -> {
            AtomicLong serverPublishingSequence = new AtomicLong(0);
            Random random = new Random();
            int outstandingMultiple = 0;
            while (true) {
                try {
                    String body = messagesSentToServer.poll(10, TimeUnit.SECONDS);
                    receivedMessages.add(body);
                    long messageSequenceNumber = serverPublishingSequence.getAndIncrement();
                    if (outstandingMultiple == 0) {
                        // no outstanding multiple
                        boolean multiple = random.nextBoolean();
                        if (multiple) {
                            outstandingMultiple = random.nextInt(10) + 1;
                        } else {
                            if (random.nextInt(100) < 5) {
                                handleNackReference.get().handle(messageSequenceNumber, false);
                            } else {
                                handleAckReference.get().handle(messageSequenceNumber, false);
                            }
                        }
                    } else if (outstandingMultiple == 1) {
                        // last outstanding acking/nacking
                        if (random.nextInt(100) < 5) {
                            handleNackReference.get().handle(messageSequenceNumber, true);
                        } else {
                            handleAckReference.get().handle(messageSequenceNumber, true);
                        }
                        outstandingMultiple = 0;
                    } else {
                        outstandingMultiple--;
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        }).start();
        return receivedMessages;
    }

    Map<Long, String> resendUnconfirmedMessages(Channel ch) throws Exception {
        ConcurrentNavigableMap<Long, String> outstandingConfirms = new ConcurrentSkipListMap<>();
        ch.confirmSelect();

        ConfirmCallback handleAck = (deliveryTag, multiple) -> {
            if (multiple) {
                ConcurrentNavigableMap<Long, String> confirmed = outstandingConfirms.headMap(deliveryTag, true);
                confirmed.clear();
            } else {
                outstandingConfirms.remove(deliveryTag);
            }
        };

        Queue<String> nackedMessages = new ConcurrentLinkedQueue<>();
        ConfirmCallback handleNack = (deliveryTag, multiple) -> {
            if (multiple) {
                ConcurrentNavigableMap<Long, String> nacked = outstandingConfirms.headMap(deliveryTag, true);
                for (String body : nacked.values()) {
                    nackedMessages.offer(body);
                }
                nacked.clear();
            } else {
                nackedMessages.add(outstandingConfirms.get(deliveryTag));
                outstandingConfirms.remove(deliveryTag);
            }
        };

        ch.addConfirmListener(handleAck, handleNack);

        for (int i = 0; i < messageCount; i++) {
            String body = String.valueOf(i);
            outstandingConfirms.put(ch.getNextPublishSeqNo(), body);
            ch.basicPublish("", queue, null, body.getBytes());
            meter.mark();
            if (!nackedMessages.isEmpty()) {
                while ((body = nackedMessages.poll()) != null) {
                    outstandingConfirms.put(ch.getNextPublishSeqNo(), body);
                    ch.basicPublish("", queue, null, body.getBytes());
                }
            }
        }
        while (!outstandingConfirms.isEmpty() && !nackedMessages.isEmpty()) {
            String body;
            while ((body = nackedMessages.poll()) != null) {
                outstandingConfirms.put(ch.getNextPublishSeqNo(), body);
                ch.basicPublish("", queue, null, body.getBytes());
            }
        }
        return outstandingConfirms;
    }

    Map<Long, String> resendUnconfirmedMessagesUseLowerBoundAndConcurrentMap(Channel ch) throws Exception {
        Map<Long, String> outstandingConfirms = new ConcurrentHashMap<>();
        AtomicLong multipleLowerBound = new AtomicLong(1);
        ch.confirmSelect();

        ConfirmCallback handleAck = (deliveryTag, multiple) -> {
            Long lowerBound = multipleLowerBound.get();
            if (multiple) {
                for (long i = lowerBound; i <= deliveryTag; i++) {
                    outstandingConfirms.remove(i);
                }
                multipleLowerBound.compareAndSet(lowerBound, deliveryTag);
            } else {
                outstandingConfirms.remove(deliveryTag);
                if (deliveryTag == lowerBound + 1) {
                    multipleLowerBound.compareAndSet(lowerBound, deliveryTag);
                }
            }
        };

        Queue<String> nackedMessages = new ConcurrentLinkedQueue<>();
        ConfirmCallback handleNack = (deliveryTag, multiple) -> {
            Long lowerBound = multipleLowerBound.get();
            if (multiple) {
                for (long i = lowerBound; i <= deliveryTag; i++) {
                    String body = outstandingConfirms.remove(i);
                    if (body != null) {
                        nackedMessages.offer(body);
                    }
                }
            } else {
                String body = outstandingConfirms.remove(deliveryTag);
                if (body != null) {
                    nackedMessages.offer(body);
                }
                if (deliveryTag == lowerBound + 1) {
                    multipleLowerBound.compareAndSet(lowerBound, deliveryTag);
                }
            }
        };

        ch.addConfirmListener(handleAck, handleNack);

        for (int i = 0; i < messageCount; i++) {
            String body = String.valueOf(i);
            outstandingConfirms.put(ch.getNextPublishSeqNo(), body);
            ch.basicPublish("", queue, null, body.getBytes());
            meter.mark();
            if (!nackedMessages.isEmpty()) {
                while ((body = nackedMessages.poll()) != null) {
                    outstandingConfirms.put(ch.getNextPublishSeqNo(), body);
                    ch.basicPublish("", queue, null, body.getBytes());
                }
            }
        }
        while (!outstandingConfirms.isEmpty() && !nackedMessages.isEmpty()) {
            String body;
            while ((body = nackedMessages.poll()) != null) {
                outstandingConfirms.put(ch.getNextPublishSeqNo(), body);
                ch.basicPublish("", queue, null, body.getBytes());
            }
        }
        return outstandingConfirms;
    }


}
