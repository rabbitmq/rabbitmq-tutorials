import com.rabbitmq.stream.ByteCapacity;
import com.rabbitmq.stream.Environment;
import com.rabbitmq.stream.OffsetSpecification;

import java.time.Duration;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;

import static java.nio.charset.StandardCharsets.UTF_8;

public class OffsetTrackingReceive {

  public static void main(String[] args) throws InterruptedException {
    try (Environment environment =
        Environment.builder().requestedHeartbeat(Duration.ofSeconds(5)).build()) {
      String stream = "stream-offset-tracking-java";
      environment.streamCreator().stream(stream).maxLengthBytes(ByteCapacity.GB(1)).create();

      OffsetSpecification offsetSpecification = OffsetSpecification.first();
      AtomicLong firstOffset = new AtomicLong(-1);
      AtomicLong lastOffset = new AtomicLong(0);
      AtomicLong messageCount = new AtomicLong(0);
      CountDownLatch consumedLatch = new CountDownLatch(1);
      environment.consumerBuilder().stream(stream)
          .offset(offsetSpecification)
          .name("offset-tracking-tutorial")
          .manualTrackingStrategy().builder()
          .messageHandler(
              (ctx, msg) -> {
                if (firstOffset.compareAndSet(-1, ctx.offset())) {
                  System.out.println("First message received.");
                }
                if (messageCount.incrementAndGet() % 10 == 0) {
                  ctx.storeOffset();
                }
                String body = new String(msg.getBodyAsBinary(), UTF_8);
                if ("marker".equals(body)) {
                  lastOffset.set(ctx.offset());
                  ctx.storeOffset();
                  ctx.consumer().close();
                  consumedLatch.countDown();
                }
              })
          .build();
      System.out.println("Started consuming...");

      consumedLatch.await(60, TimeUnit.MINUTES);

      System.out.printf("Done consuming, first offset %d, last offset %d.%n",
          firstOffset.get(), lastOffset.get());
    }
  }

}
