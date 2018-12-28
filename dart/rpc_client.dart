import "dart:io";
import "dart:async";
import "dart:math";
import "package:dart_amqp/dart_amqp.dart";

var UUID = () => "${(new Random()).nextDouble()}";

class RPCClient {
    Client client;
    String queueTag;
    String _replyQueueTag;
    Completer contextChannel;
    Map<String, Completer> _channels = new Map<String, Completer>();
    Queue _queue;
    RPCClient() : client = new Client(),
        queueTag = "rpc_queue" {
        contextChannel = new Completer();
        client
        .channel()
        .then((Channel channel) => channel.queue(queueTag))
        .then((Queue rpcQueue) {
            _queue = rpcQueue;
            return rpcQueue.channel.privateQueue();
        })
        .then((Queue rpcQueue) {
            rpcQueue.consume(noAck: true)
                .then((Consumer consumer) {
                    _replyQueueTag = consumer.queue.name;
                    consumer.listen(handler);
                    contextChannel.complete();
                });
        });
    }

    void handler (AmqpMessage event) {
        if (!_channels
            .containsKey(
                event.properties.corellationId)) return;
        print(" [.] Got ${event.payloadAsString}");
        _channels
            .remove(event.properties.corellationId)
            .complete(event.payloadAsString);
    }

    Future<String> call(int n) {
        return contextChannel.future
            .then((_) {
                String uuid = UUID();
                Completer<String> channel = new Completer<String>();
                MessageProperties properties = new MessageProperties()
                    ..replyTo = _replyQueueTag
                    ..corellationId = uuid;
                _channels[uuid] = channel;
                print(" [x] Requesting ${n}");
                _queue.publish({"n": n}, properties: properties);
                return channel.future;
            });
    }

    Future close() {
        _channels.forEach((_, var next) => next.complete("RPC client closed"));
        _channels.clear();
        client.close();
    }
}

void main(List<String> arguments) {
    if (arguments.isEmpty) {
        print("Usage: rpc_client.dart num");
        return;
    }
    RPCClient client = new RPCClient();
    int n = arguments.isEmpty ? 30 : num.parse(arguments[0]);
    client.call(n)
        .then((String res) {
            print(" [x] fib(${n}) = ${res}");
        })
        .then((_) => client.close())
        .then((_) => null);
}