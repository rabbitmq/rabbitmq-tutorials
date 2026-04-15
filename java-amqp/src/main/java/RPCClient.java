import com.rabbitmq.client.amqp.Connection;
import com.rabbitmq.client.amqp.Environment;
import com.rabbitmq.client.amqp.Message;
import com.rabbitmq.client.amqp.Requester;

import java.nio.charset.StandardCharsets;
import java.util.concurrent.TimeUnit;

public class RPCClient {

    public static void main(String[] argv) throws Exception {
        Environment environment = TutorialSupport.newEnvironment();
        try (Connection connection = environment.connectionBuilder().uri(TutorialSupport.BROKER_URI).build();
             Requester requester = connection.requesterBuilder()
                     .requestAddress().queue("rpc_queue")
                     .requester()
                     .build()) {
            for (int i = 0; i < 32; i++) {
                String iStr = Integer.toString(i);
                System.out.println(" [x] Requesting fib(" + iStr + ")");
                Message request = requester.message(iStr.getBytes(StandardCharsets.UTF_8));
                Message reply = requester.publish(request).get(30, TimeUnit.SECONDS);
                String response = new String(reply.body(), StandardCharsets.UTF_8);
                System.out.println(" [.] Got '" + response + "'");
            }
        } finally {
            environment.close();
        }
    }
}
