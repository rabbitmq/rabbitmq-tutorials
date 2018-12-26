import "package:dart_amqp/dart_amqp.dart";

void main (List<String> arguments) {
    ConnectionSettings settings = new ConnectionSettings(
        host: "localhost"
    );

    Client client = new Client(settings: settings);

    String msg = arguments.isEmpty ? "Hello World!" : arguments[0];

    client
        .channel()
        .then((Channel channel) =>
                channel.exchange("logs", ExchangeType.FANOUT, durable: false))
        .then((Exchange exchange) {
            exchange.publish(msg, null);
            print(" [x] Sent ${msg}");
            return client.close();
        });
}
