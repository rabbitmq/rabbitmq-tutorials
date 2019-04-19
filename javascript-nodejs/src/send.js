#!/usr/bin/env node

var amqp = require('amqplib/callback_api');

amqp.connect('amqp://localhost', function(error, connection) {
  if (error) throw error;
  connection.createChannel(function(err, channel) {
    if (err) throw err;
    var queue = 'hello';
    var msg = 'Hello World!';

    channel.assertQueue(queue, {durable: false});
    channel.sendToQueue(queue, Buffer.from(msg));
    console.log(" [x] Sent %s", msg);
  });
  setTimeout(function() { connection.close(); process.exit(0) }, 500);
});
