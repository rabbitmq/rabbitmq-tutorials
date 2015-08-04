#!/usr/bin/env node

var amqp = require('amqplib');
var when = require('when');

var conn = amqp.connect('amqp://localhost')
conn.then(createChannel).then(null, console.warn);

function createChannel(conn) {
  return when(conn.createChannel().then(logMessage)).ensure(function() { conn.close(); });
}

function logMessage(ch) {
  var ex = 'logs';
  var ok = ch.assertExchange(ex, 'fanout', {durable: false})

  return ok.then(function() {
    var msg = process.argv.slice(2).join(' ') || 'Hello World!';

    ch.publish(ex, '', new Buffer(msg));
    console.log(" [x] Sent '%s'", msg);
    return ch.close();
  });
}
