import "dart:io";
import "package:dart_amqp/dart_amqp.dart";

void main (List<String> arguments) {
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

    String consumeTag = "task_queue";

    client
        .channel()
        .then((Channel channel) {
            return channel.qos(0, 1)
                .then((Channel channel) =>
                                channel.queue(consumeTag, durable: true));
        })
        .then((Queue queue) => queue.consume(noAck: false))
        .then((Consumer consumer) {
            consumer.listen((AmqpMessage event) {
                String payload = event.payloadAsString;
                print(" [x] Received ${payload}");
                sleep(new Duration(seconds : payload.split(".").length));
                print(" [x] Done");
                event.ack();
            });
        });
}
