import "package:dart_amqp/dart_amqp.dart";

void main (List<String> arguments) {
    ConnectionSettings settings = new ConnectionSettings(
        host: "localhost"
    );

    Client client = new Client(settings: settings);

    String consumeTag = "hello";
    String msg = "hello";

    client
        .channel()
        .then((Channel channel) {
            return channel.queue(consumeTag, durable: false);
        })
        .then((Queue queue) {
            queue.publish(msg);
            print(" [x] Sent ${msg}");
            client.close();
        });
}
