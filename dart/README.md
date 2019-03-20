# Dart code for RabbitMQ tutorials

Here you can find an [Dart](https://www.dartlang.org/) port of
[RabbitMQ tutorials](https://www.rabbitmq.com/getstarted.html).


## Requirements

To run this code you need a [Dart 2 server platform installed](https://www.dartlang.org/tools/sdk#install)

### Dart 2.0+

These tutorials use [dart_amqp](https://github.com/achilleasa/dart_amqp).

To install dependencies with pub, run:

    pub get

## Code

To run the examples, use `dart source_file.dart`.

Tutorial one: "Hello World!":

    dart receive.dart
    dart send.dart

Tutorial two: Work Queues

    dart worker.dart
    dart new_task.dart

Tutorial three: Publish/Subscribe

    dart receive_logs.dart
    dart emit_log.dart

Tutorial four: Routing

    dart receive_logs_direct.dart info warning
    dart emit_log_direct.dart info "A message"
    dart emit_log_direct.dart warning "A warning"

Tutorial five: Topics

    dart receive_logs_topic.dart "info.*" "warn.*"
    dart emit_log_topic.dart "info.connections" "Connected"
    dart emit_log_topic.dart "warn.connecctions" "A warning"

Tutorial six: RPC (Request/Response)

    dart rpc_server.dart
    dart rpc_client.dart
