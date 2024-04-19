import com.rabbitmq.stream.ByteCapacity;
import com.rabbitmq.stream.Consumer;
import com.rabbitmq.stream.Environment;
import com.rabbitmq.stream.OffsetSpecification;

import java.io.IOException;

public class Receive {

    public static void main(String[] args) throws IOException {
        Environment environment = Environment.builder().build();
        String stream = "hello-java-stream";
        environment.streamCreator().stream(stream).maxLengthBytes(ByteCapacity.GB(5)).create();

        Consumer consumer = environment.consumerBuilder()
            .stream(stream)
            .offset(OffsetSpecification.first())
            .messageHandler((unused, message) -> {
                System.out.println("Received message: " + new String(message.getBodyAsBinary()));
            }).build();

        System.out.println(" [x]  Press Enter to close the consumer...");
        System.in.read();
        consumer.close();
        environment.close();
    }
}
