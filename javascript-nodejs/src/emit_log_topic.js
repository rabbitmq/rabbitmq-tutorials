#!/usr/bin/env node

var amqp = require('amqplib/callback_api');

amqp.connect('amqp://localhost', function(err, conn) {
  conn.createChannel(function(err, ch) {
    var ex = 'topic_logs';
    var args = process.argv.slice(2);
    var key = (args.length > 0) ? args[0] : 'anonymous.info';
    var msg = args.slice(1).join(' ') || 'Hello World!';

    ch.assertExchange(ex, 'topic', {durable: false});
    ch.publish(ex, key, new Buffer(msg));
    console.log(" [x] Sent %s: '%s'", key, msg);
  });

  setTimeout(function() { conn.close(); process.exit(0) }, 500);
});
