#!/usr/bin/env node

usage();

var amqp = require('amqplib');
var all = require('when').all;
var severities = process.argv.slice(2);
var conn = amqp.connect('amqp://localhost')
var ch = conn.then(createChannel).then(null, console.warn);

ch.then(function(ch) {
  var x = ch.assertExchange('direct_logs', 'direct', {durable: false});
  var q = x.then(function() {
    return ch.assertQueue('', {exclusive: true});
  });

  var ok = q.then(function(qok) {
    var queue = qok.queue;
    return all(severities.map(function(sev) {
      ch.bindQueue(queue, 'direct_logs', sev);
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
    console.log("Usage: receive_logs_direct.js [info] [warning] [error]");
    process.exit(1);
  }
}
