import "package:dart_amqp/dart_amqp.dart";

void main(List<String> arguments) {
    ConnectionSettings settings = new ConnectionSettings(
        host: "localhost"
    );

    Client client = new Client(settings: settings);

    String routingKey = arguments.length < 1 ? "anonymous.info" : arguments[0];
    String msg = arguments.length < 2 ? "Hello World!" : arguments[1];

    client
        .channel()
        .then((Channel channel) =>
                channel.exchange("topic_logs", ExchangeType.TOPIC,
                        durable: false))
        .then((Exchange exchange) {
            exchange.publish(msg, routingKey);
            print(" [x] Sent ${routingKey}: ${msg}");
            return client.close();
        });
}
