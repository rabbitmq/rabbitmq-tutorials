#!/usr/bin/env node

usage();

var amqp = require('amqplib');
var all = require('when').all;
var conn = amqp.connect('amqp://localhost')
var ch = conn.then(createChannel).then(null, console.warn);

ch.then(function(ch) {
  var x = ch.assertExchange('topic_logs', 'topic', {durable: false});
  var q = x.then(function() {
    return ch.assertQueue('', {exclusive: true});
  });

  var ok = q.then(function(qok) {
    var queue = qok.queue;
    var keys = process.argv.slice(2);

    return all(keys.map(function(key) {
      ch.bindQueue(queue, 'topic_logs', key);
    })).then(function() { return queue; });
  });

  ok = ok.then(function(queue) {
    return ch.consume(queue, logMessage, {noAck: true});
  });
  return ok.then(function() {
    console.log(' [*] Waiting for logs. To exit press CTRL+C');
  });

  function logMessage(msg) {
    console.log(" [x] %s:'%s'", msg.fields.routingKey, msg.content.toString());
  }
});

function createChannel(conn) {
  process.once('SIGINT', function() { conn.close(); });
  return conn.createChannel();
}

function usage() {
  if (process.argv.length < 3) {
    console.log("Usage: receive_logs_topic.js binding_key [binding_key...]");
    process.exit(1);
  }
}
