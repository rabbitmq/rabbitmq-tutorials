#!/usr/bin/env node

var amqp = require('amqplib/callback_api');

amqp.connect('amqp://localhost', function(err, conn) {
  conn.createChannel(function(err, ch) {
    var q = 'hello';
    var buffer = new Buffer('Hello World!');

    ch.assertQueue(q, {durable: false});
    ch.sendToQueue(q, buffer);
    console.log(" [x] Sent %s", buffer.toString());
  });
  setTimeout(function() { conn.close(); process.exit(0) }, 500);
});
