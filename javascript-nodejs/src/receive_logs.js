#!/usr/bin/env node

var amqp = require('amqplib');
var conn = amqp.connect('amqp://localhost')
var ch = conn.then(createChannel).then(null, console.warn);

ch.then(function(ch) {
  var x = ch.assertExchange('logs', 'fanout', {durable: false});
  var q = x.then(function() {
    return ch.assertQueue('', {exclusive: true});
  });

  ok = q.then(function(qok) {
    return ch.bindQueue(qok.queue, 'logs', '').then(function() {
      return qok.queue;
    });
  });
  ok = ok.then(function(queue) {
    return ch.consume(queue, logMessage, {noAck: true});
  });
  return ok.then(function() {
    console.log(' [*] Waiting for logs. To exit press CTRL+C');
  });

  function logMessage(msg) {
    console.log(" [x] '%s'", msg.content.toString());
  }
});

function createChannel(conn) {
  process.once('SIGINT', function() { conn.close(); });
  return conn.createChannel();
}
