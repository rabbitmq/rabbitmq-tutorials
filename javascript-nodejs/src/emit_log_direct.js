#!/usr/bin/env node

var amqp = require('amqplib/callback_api');

amqp.connect('amqp://localhost', function(error, connection) {
  if (error) throw error;
  connection.createChannel(function(err, channel) {
    if (err) throw err;
    var exchange = 'direct_logs';
    var args = process.argv.slice(2);
    var msg = args.slice(1).join(' ') || 'Hello World!';
    var severity = (args.length > 0) ? args[0] : 'info';

    channel.assertExchange(exchange, 'direct', {durable: false});
    channel.publish(exchange, severity, Buffer.from(msg));
    console.log(" [x] Sent %s: '%s'", severity, msg);
  });

  setTimeout(function() { connection.close(); process.exit(0) }, 500);
});
