import com.rabbitmq.stream.*;
import java.io.IOException;

public class Send {

    public static void main(String[] args) throws IOException {
        Environment environment = Environment.builder().build();
        String stream = "hello-java-stream";
        environment.streamCreator().stream(stream).maxLengthBytes(ByteCapacity.GB(5)).create();

        Producer producer = environment.producerBuilder().stream(stream).build();
        producer.send(producer.messageBuilder().addData("Hello, World!".getBytes()).build(), null);
        System.out.println(" [x] 'Hello, World!' message sent");

        System.out.println(" [x] Press Enter to close the producer...");
        System.in.read();
        producer.close();
        environment.close();
    }
}
