import "package:dart_amqp/dart_amqp.dart";

void main(List<String> arguments) {
    ConnectionSettings settings = new ConnectionSettings(
        host: "localhost"
    );

    Client client = new Client(settings: settings);

    String consumeTag = "task_queue";
    String msg = arguments.isEmpty ? "Hello World!" : arguments[0];

    MessageProperties properties = new MessageProperties.persistentMessage();

    client
        .channel()
        .then((Channel channel) =>
                channel.queue(consumeTag, durable: true))
        .then((Queue queue) {
            queue.publish(msg, properties: properties);
            print(" [x] Sent ${msg}");
            return client.close();
        });
}
