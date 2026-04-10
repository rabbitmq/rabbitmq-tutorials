import com.rabbitmq.client.amqp.Connection;
import com.rabbitmq.client.amqp.Environment;
import com.rabbitmq.client.amqp.Management;
import com.rabbitmq.client.amqp.Responder;

import java.nio.charset.StandardCharsets;

public class RPCServer {

    private static final String RPC_QUEUE_NAME = "rpc_queue";

    private static int fib(int n) {
        if (n == 0) {
            return 0;
        }
        if (n == 1) {
            return 1;
        }
        return fib(n - 1) + fib(n - 2);
    }

    public static void main(String[] argv) throws Exception {
        Environment environment = TutorialSupport.newEnvironment();
        Connection connection = environment.connectionBuilder().uri(TutorialSupport.BROKER_URI).build();
        try (Management management = connection.management()) {
            management.queue().name(RPC_QUEUE_NAME).quorum().queue().declare();
            management.queuePurge(RPC_QUEUE_NAME);
        }

        System.out.println(" [x] Awaiting RPC requests");

        Responder responder = connection.responderBuilder()
                .requestQueue(RPC_QUEUE_NAME)
                .handler((ctx, req) -> {
                    String response = "";
                    try {
                        String message = new String(req.body(), StandardCharsets.UTF_8);
                        int n = Integer.parseInt(message);
                        System.out.println(" [.] fib(" + message + ")");
                        response += fib(n);
                    } catch (RuntimeException e) {
                        System.out.println(" [.] " + e);
                    }
                    return ctx.message(response.getBytes(StandardCharsets.UTF_8));
                })
                .build();

        Runtime.getRuntime().addShutdownHook(new Thread(() -> {
            responder.close();
            connection.close();
            environment.close();
        }));

        Thread.sleep(Long.MAX_VALUE);
    }
}
