#!/usr/bin/env node

var amqp = require('amqplib');
var when = require('when');

var conn = amqp.connect('amqp://localhost')
conn.then(createChannel).then(null, console.warn);

function createChannel(conn) {
  return when(conn.createChannel().then(logMessage)).ensure(function() { conn.close(); });
}

function logMessage(ch) {
  var ex = 'topic_logs';
  var ok = ch.assertExchange(ex, 'topic', {durable: false})

  return ok.then(function() {
    var args = process.argv.slice(2);
    var msg = args.slice(1).join(' ') || 'Hello World!';
    var key = (args.length > 0) ? args[0] : 'anonymous.info';

    ch.publish(ex, key, new Buffer(msg));
    console.log(" [x] Sent %s:'%s'", key, msg);
    return ch.close();
  });
}
