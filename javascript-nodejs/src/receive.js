#!/usr/bin/env node

var amqp = require('amqplib');

var conn = amqp.connect('amqp://localhost');
conn.then(createChannel).then(null, console.warn);

function createChannel(conn) {
  process.once('SIGINT', function() { conn.close(); });
  return conn.createChannel().then(consume);
}

function consume(ch) {
  var ok = ch.assertQueue('hello', {durable: false});

  ok = ok.then(function(_ignore) {
    return ch.consume('hello', function(msg) {
      console.log(" [x] Received '%s'", msg.content.toString());
    }, {noAck: true});
  });

  return ok.then(function(_consumeOk) {
    console.log(' [*] Waiting for messages. To exit press CTRL+C');
  });
}
