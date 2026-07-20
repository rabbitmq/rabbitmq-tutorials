import "dart:io";
import "package:dart_amqp/dart_amqp.dart";

int fib(int n) {
  int a = 0;
  int b = 1;
  for (var i = 0; i < n; i++) {
    final next = a + b;
    a = b;
    b = next;
  }
  return a;
}

void main(List<String> args) {

  Client client = new Client();

  // Setup a signal handler to cleanly exit if CTRL+C is pressed
  ProcessSignal.sigint.watch().listen((_) {
    client.close().then((_) {
      exit(0);
    });
  });

  client
  .channel()
  .then((Channel channel) => channel.qos(0, 1))
  .then((Channel channel) => channel.queue("rpc_queue"))
  .then((Queue queue) => queue.consume())
  .then((Consumer consumer) {
    print(" [x] Awaiting RPC request");
    consumer.listen((AmqpMessage message) {
      var n = message.payloadAsJson["n"];
      print(" [.] fib(${n})");
      message.reply(fib(n).toString());
    });
  });
}
