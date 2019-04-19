#!/usr/bin/env node

var amqp = require('amqplib/callback_api');

amqp.connect('amqp://localhost', function(error, connection) {
  if (error) throw error;
  connection.createChannel(function(erro, channel) {
    if (erro) throw erro;
    var exchange = 'logs';

    channel.assertExchange(exchange, 'fanout', {durable: false});

    channel.assertQueue('', {exclusive: true}, function(err, q) {
      if (err) throw err;
      console.log(" [*] Waiting for messages in %s. To exit press CTRL+C", q.queue);
      channel.bindQueue(q.queue, exchange, '');

      channel.consume(q.queue, function(msg) {
        if(msg.content) {
          console.log(" [x] %s", msg.content.toString());
        }
      }, {noAck: true});
    });
  });
});
