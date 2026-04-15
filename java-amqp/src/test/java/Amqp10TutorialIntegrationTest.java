import com.rabbitmq.client.amqp.Connection;
import com.rabbitmq.client.amqp.Consumer;
import com.rabbitmq.client.amqp.Environment;
import com.rabbitmq.client.amqp.Management;
import com.rabbitmq.client.amqp.Management.QueueInfo;
import com.rabbitmq.client.amqp.Message;
import com.rabbitmq.client.amqp.Publisher;
import com.rabbitmq.client.amqp.Requester;
import com.rabbitmq.client.amqp.Responder;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;

import java.nio.charset.StandardCharsets;
import java.util.UUID;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Integration tests against a RabbitMQ node on localhost (same requirement as running the tutorials).
 */
class Amqp10TutorialIntegrationTest {

    @Test
    @Timeout(60)
    void tutorial1_helloWorld() throws Exception {
        Environment environment = TutorialSupport.newEnvironment();
        try (Connection connection = environment.connectionBuilder().uri(TutorialSupport.BROKER_URI).build()) {
            String queue = "hello_it_" + UUID.randomUUID();
            try (Management management = connection.management()) {
                management.queue().name(queue).quorum().queue().declare();
            }
            BlockingQueue<String> received = new LinkedBlockingQueue<>();
            Consumer consumer = connection.consumerBuilder()
                    .queue(queue)
                    .messageHandler((ctx, msg) -> {
                        received.offer(new String(msg.body(), StandardCharsets.UTF_8));
                        ctx.accept();
                    })
                    .build();
            try (Publisher publisher = connection.publisherBuilder().queue(queue).build()) {
                CountDownLatch ok = new CountDownLatch(1);
                Message m = publisher.message("Hello World!".getBytes(StandardCharsets.UTF_8));
                publisher.publish(m, ctx -> {
                    if (ctx.status() == Publisher.Status.ACCEPTED) {
                        ok.countDown();
                    }
                });
                assertThat(ok.await(30, TimeUnit.SECONDS)).isTrue();
            }
            assertThat(received.poll(30, TimeUnit.SECONDS)).isEqualTo("Hello World!");
            consumer.close();
            try (Management management = connection.management()) {
                management.queueDelete(queue);
            }
        } finally {
            environment.close();
        }
    }

    @Test
    @Timeout(120)
    void tutorial2_workQueueFairDispatch() throws Exception {
        Environment environment = TutorialSupport.newEnvironment();
        try (Connection connection = environment.connectionBuilder().uri(TutorialSupport.BROKER_URI).build()) {
            String queue = "task_it_" + UUID.randomUUID();
            try (Management management = connection.management()) {
                management.queue().name(queue).quorum().queue().declare();
            }
            CountDownLatch processed = new CountDownLatch(3);
            Consumer consumer = connection.consumerBuilder()
                    .queue(queue)
                    .initialCredits(1)
                    .messageHandler((ctx, msg) -> {
                        processed.countDown();
                        ctx.accept();
                    })
                    .build();
            try (Publisher publisher = connection.publisherBuilder().queue(queue).build()) {
                for (int k = 0; k < 3; k++) {
                    CountDownLatch ok = new CountDownLatch(1);
                    Message m = publisher.message(("m" + k).getBytes(StandardCharsets.UTF_8));
                    publisher.publish(m, ctx -> {
                        if (ctx.status() == Publisher.Status.ACCEPTED) {
                            ok.countDown();
                        }
                    });
                    assertThat(ok.await(30, TimeUnit.SECONDS)).isTrue();
                }
            }
            assertThat(processed.await(30, TimeUnit.SECONDS)).isTrue();
            consumer.close();
            try (Management management = connection.management()) {
                management.queueDelete(queue);
            }
        } finally {
            environment.close();
        }
    }

    @Test
    @Timeout(60)
    void tutorial3_publishSubscribe() throws Exception {
        String ex = "logs_it_" + UUID.randomUUID();
        Environment environment = TutorialSupport.newEnvironment();
        try (Connection connection = environment.connectionBuilder().uri(TutorialSupport.BROKER_URI).build()) {
            String queue;
            try (Management management = connection.management()) {
                management.exchange().name(ex).type(Management.ExchangeType.FANOUT).declare();
                QueueInfo q = management.queue().exclusive(true).autoDelete(true).declare();
                queue = q.name();
                management.binding().sourceExchange(ex).destinationQueue(queue).key("").bind();
            }
            BlockingQueue<String> received = new LinkedBlockingQueue<>();
            Consumer consumer = connection.consumerBuilder()
                    .queue(queue)
                    .messageHandler((ctx, msg) -> {
                        received.offer(new String(msg.body(), StandardCharsets.UTF_8));
                        ctx.accept();
                    })
                    .build();
            try (Publisher publisher = connection.publisherBuilder().exchange(ex).build()) {
                CountDownLatch ok = new CountDownLatch(1);
                Message m = publisher.message("broadcast".getBytes(StandardCharsets.UTF_8));
                publisher.publish(m, ctx -> {
                    if (ctx.status() == Publisher.Status.ACCEPTED) {
                        ok.countDown();
                    }
                });
                assertThat(ok.await(30, TimeUnit.SECONDS)).isTrue();
            }
            assertThat(received.poll(30, TimeUnit.SECONDS)).isEqualTo("broadcast");
            consumer.close();
            try (Management management = connection.management()) {
                management.exchangeDelete(ex);
            }
        } finally {
            environment.close();
        }
    }

    @Test
    @Timeout(60)
    void tutorial4_routing() throws Exception {
        String ex = "direct_it_" + UUID.randomUUID();
        Environment environment = TutorialSupport.newEnvironment();
        try (Connection connection = environment.connectionBuilder().uri(TutorialSupport.BROKER_URI).build()) {
            String queue;
            try (Management management = connection.management()) {
                management.exchange().name(ex).type(Management.ExchangeType.DIRECT).declare();
                QueueInfo q = management.queue().exclusive(true).autoDelete(true).declare();
                queue = q.name();
                management.binding().sourceExchange(ex).destinationQueue(queue).key("info").bind();
            }
            BlockingQueue<String> keys = new LinkedBlockingQueue<>();
            BlockingQueue<String> bodies = new LinkedBlockingQueue<>();
            Consumer consumer = connection.consumerBuilder()
                    .queue(queue)
                    .messageHandler((ctx, msg) -> {
                        Object ann = msg.annotation("x-routing-key");
                        String rk = ann != null ? ann.toString() : (msg.subject() != null ? msg.subject() : "");
                        keys.offer(rk);
                        bodies.offer(new String(msg.body(), StandardCharsets.UTF_8));
                        ctx.accept();
                    })
                    .build();
            try (Publisher publisher = connection.publisherBuilder().exchange(ex).key("info").build()) {
                CountDownLatch ok = new CountDownLatch(1);
                Message m = publisher.message("Hello".getBytes(StandardCharsets.UTF_8));
                publisher.publish(m, ctx -> {
                    if (ctx.status() == Publisher.Status.ACCEPTED) {
                        ok.countDown();
                    }
                });
                assertThat(ok.await(30, TimeUnit.SECONDS)).isTrue();
            }
            assertThat(keys.poll(30, TimeUnit.SECONDS)).isEqualTo("info");
            assertThat(bodies.poll(30, TimeUnit.SECONDS)).isEqualTo("Hello");
            consumer.close();
            try (Management management = connection.management()) {
                management.exchangeDelete(ex);
            }
        } finally {
            environment.close();
        }
    }

    @Test
    @Timeout(60)
    void tutorial5_topics() throws Exception {
        String ex = "topic_it_" + UUID.randomUUID();
        Environment environment = TutorialSupport.newEnvironment();
        try (Connection connection = environment.connectionBuilder().uri(TutorialSupport.BROKER_URI).build()) {
            String queue;
            try (Management management = connection.management()) {
                management.exchange().name(ex).type(Management.ExchangeType.TOPIC).declare();
                QueueInfo q = management.queue().exclusive(true).autoDelete(true).declare();
                queue = q.name();
                management.binding().sourceExchange(ex).destinationQueue(queue).key("*.critical").bind();
            }
            BlockingQueue<String> keys = new LinkedBlockingQueue<>();
            Consumer consumer = connection.consumerBuilder()
                    .queue(queue)
                    .messageHandler((ctx, msg) -> {
                        Object ann = msg.annotation("x-routing-key");
                        keys.offer(ann != null ? ann.toString() : (msg.subject() != null ? msg.subject() : ""));
                        ctx.accept();
                    })
                    .build();
            try (Publisher publisher = connection.publisherBuilder().exchange(ex).key("kern.critical").build()) {
                CountDownLatch ok = new CountDownLatch(1);
                Message m = publisher.message("A critical kernel error".getBytes(StandardCharsets.UTF_8));
                publisher.publish(m, ctx -> {
                    if (ctx.status() == Publisher.Status.ACCEPTED) {
                        ok.countDown();
                    }
                });
                assertThat(ok.await(30, TimeUnit.SECONDS)).isTrue();
            }
            assertThat(keys.poll(30, TimeUnit.SECONDS)).isEqualTo("kern.critical");
            consumer.close();
            try (Management management = connection.management()) {
                management.exchangeDelete(ex);
            }
        } finally {
            environment.close();
        }
    }

    @Test
    @Timeout(60)
    void tutorial6_rpc() throws Exception {
        String queue = "rpc_it_" + UUID.randomUUID();
        Environment environment = TutorialSupport.newEnvironment();
        try (Connection connection = environment.connectionBuilder().uri(TutorialSupport.BROKER_URI).build()) {
            try (Management management = connection.management()) {
                management.queue().name(queue).quorum().queue().declare();
            }
            Responder responder = connection.responderBuilder()
                    .requestQueue(queue)
                    .handler((ctx, req) -> {
                        int n = Integer.parseInt(new String(req.body(), StandardCharsets.UTF_8));
                        int fib = fib(n);
                        return ctx.message(String.valueOf(fib).getBytes(StandardCharsets.UTF_8));
                    })
                    .build();
            try (Requester requester = connection.requesterBuilder()
                    .requestAddress().queue(queue)
                    .requester()
                    .build()) {
                Message reply = requester.publish(requester.message("10".getBytes(StandardCharsets.UTF_8)))
                        .get(30, TimeUnit.SECONDS);
                assertThat(new String(reply.body(), StandardCharsets.UTF_8)).isEqualTo("55");
            } finally {
                responder.close();
            }
            try (Management management = connection.management()) {
                management.queueDelete(queue);
            }
        } finally {
            environment.close();
        }
    }

    private static int fib(int n) {
        if (n == 0) {
            return 0;
        }
        if (n == 1) {
            return 1;
        }
        return fib(n - 1) + fib(n - 2);
    }

    @Test
    @Timeout(120)
    void tutorial7_publisherConfirms() throws Exception {
        Environment environment = TutorialSupport.newEnvironment();
        try (Connection connection = environment.connectionBuilder().uri(TutorialSupport.BROKER_URI).build()) {
            String queue = UUID.randomUUID().toString();
            try (Management management = connection.management()) {
                management.queue().name(queue).exclusive(true).autoDelete(true).declare();
            }
            int nMsg = 500;
            CountDownLatch confirmed = new CountDownLatch(nMsg);
            try (Publisher publisher = connection.publisherBuilder().queue(queue).build()) {
                for (int i = 0; i < nMsg; i++) {
                    String body = String.valueOf(i);
                    Message m = publisher.message(body.getBytes(StandardCharsets.UTF_8));
                    publisher.publish(m, ctx -> {
                        if (ctx.status() == Publisher.Status.ACCEPTED) {
                            confirmed.countDown();
                        }
                    });
                }
                assertThat(confirmed.await(60, TimeUnit.SECONDS)).isTrue();
            }
        } finally {
            environment.close();
        }
    }
}
