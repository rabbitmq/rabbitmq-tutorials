#!/usr/bin/env node

var amqp = require('amqplib');
var when = require('when');

var conn = amqp.connect('amqp://localhost');
conn.then(createChannel).then(null, console.warn);

function createChannel(conn) {
  return when(
    conn.createChannel().
    then(sendMessage)).
    ensure(function() {
      conn.close();
    });
}

function sendMessage(ch) {
  var q = 'task_queue';
  var ok = ch.assertQueue(q, {durable: true});

  return ok.then(function(_ignore) {
    var msg = process.argv.slice(2).join(' ') || "Hello World!";
    ch.sendToQueue(q, new Buffer(msg), {persistent: true});
    console.log(" [x] Sent '%s'", msg);
    return ch.close();
  });
}
