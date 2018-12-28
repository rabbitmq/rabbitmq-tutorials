import "dart:io";
import "package:dart_amqp/dart_amqp.dart";

void main (List<String> arguments) {
    if (arguments.isEmpty) {
        print("Usage: receive_logs_direct.dart <topic> [<topic>, ...]");
        return;
    }

    ConnectionSettings settings = new ConnectionSettings(
        host: "localhost"
    );

    Client client = new Client(settings: settings);

    ProcessSignal.sigint.watch().listen((_) {
        client.close().then((_) {
            print("close client");
            exit(0);
        });
    });

    List<String> routingKeys = arguments.sublist(0);
    client
        .channel()
        .then((Channel channel) {
            return channel.exchange("topic_logs", ExchangeType.TOPIC, durable: false);
        })
        .then((Exchange exchange) {
            print(" [*] Waiting for messages in logs. To Exit press CTRL+C");
            return exchange.bindPrivateQueueConsumer(routingKeys,
                consumerTag: "topic_logs", noAck: true
            );
        })
        .then((Consumer consumer) {
            consumer.listen((AmqpMessage event) {
                print(" [x] ${event.routingKey}:'${event.payloadAsString}'");
            });
        });
}
