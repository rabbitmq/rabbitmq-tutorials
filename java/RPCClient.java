import com.rabbitmq.client.ConnectionFactory;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.Channel;
import com.rabbitmq.client.DefaultConsumer;
import com.rabbitmq.client.AMQP;
import com.rabbitmq.client.Envelope;

import java.io.IOException;
import java.util.UUID;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.TimeoutException;

public class RPCClient {

  private final Connection connection;
  private final Channel channel;
  private final RpcConsumer consumer;
  private final String requestQueueName = "rpc_queue";
  private final String replyQueueName;
  private final BlockingQueue<String> response = new ArrayBlockingQueue<String>(1);

  public RPCClient() throws IOException, TimeoutException {
    ConnectionFactory factory = new ConnectionFactory();
    factory.setHost("localhost");

    connection = factory.newConnection();
    channel = connection.createChannel();
    replyQueueName = channel.queueDeclare().getQueue();
    consumer = new RpcConsumer(channel, response);
    channel.basicConsume(replyQueueName, true, consumer);
  }

  public String call(String message) throws IOException, InterruptedException {
    final String corrId = UUID.randomUUID().toString();

    consumer.setCorrelationId(corrId);

    AMQP.BasicProperties props = new AMQP.BasicProperties
            .Builder()
            .correlationId(corrId)
            .replyTo(replyQueueName)
            .build();

    channel.basicPublish("", requestQueueName, props, message.getBytes("UTF-8"));

    return response.take();
  }

  public void close() throws IOException {
    connection.close();
  }

  private static class RpcConsumer extends DefaultConsumer {
    final BlockingQueue<String> response;
    private String rpcCorrId;

    public RpcConsumer(Channel channel, BlockingQueue<String> response) {
      super(channel);
      this.response = response;
    }

    public void setCorrelationId(String rpcCorrId) {
      this.rpcCorrId = rpcCorrId;
    }

    @Override
    public void handleDelivery(String consumerTag, Envelope envelope, AMQP.BasicProperties properties, byte[] body) throws IOException {
      final String msgCorrId = properties.getCorrelationId();
      if (msgCorrId.equals(this.rpcCorrId)) {
        response.offer(new String(body, "UTF-8"));
      }
    }
  }

  public static void main(String[] argv) {
    RPCClient fibonacciRpc = null;
    String response = null;
    try {
      fibonacciRpc = new RPCClient();

      System.out.println(" [x] Requesting fib(30)");
      response = fibonacciRpc.call("30");
      System.out.println(" [.] Got '" + response + "'");

      System.out.println(" [x] Requesting fib(40)");
      response = fibonacciRpc.call("40");
      System.out.println(" [.] Got '" + response + "'");
    }
    catch  (IOException | TimeoutException | InterruptedException e) {
      e.printStackTrace();
    }
    finally {
      if (fibonacciRpc!= null) {
        try {
          fibonacciRpc.close();
        }
        catch (IOException _ignore) {}
      }
    }
  }
}

