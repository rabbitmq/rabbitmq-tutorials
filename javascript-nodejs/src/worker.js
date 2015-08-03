#!/usr/bin/env node

var amqp = require('amqplib');

var conn = amqp.connect('amqp://localhost');
conn.then(createChannel).then(null, console.warn);

function createChannel(conn) {
  process.once('SIGINT', function() { conn.close(); });
  return conn.createChannel().then(consume);
}

function consume(ch) {
  var ok = ch.assertQueue('task_queue', {durable: true});

  ok = ok.then(function() { ch.prefetch(1); });
  ok = ok.then(function(_ignore) {
    return ch.consume(
      'task_queue',
      function doSomeWork(msg) {
        console.log(" [x] Received '%s'", msg.content.toString());
        var secs = msg.content.toString().split('.').length - 1;
        setTimeout(function() {
          console.log(" [x] Done");
          ch.ack(msg);
        }, secs * 1000);
      },
      {noAck: false});
  });

  return ok.then(function(_consumeOk) {
    console.log(' [*] Waiting for messages. To exit press CTRL+C');
  });
}
