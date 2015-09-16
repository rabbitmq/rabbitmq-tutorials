#!/usr/bin/env node

var amqp = require('amqplib/callback_api');

amqp.connect('amqp://localhost', function(err, conn) {
  conn.createChannel(function(err, ch) {
    var q = 'hello';

    ch.assertQueue(q, {durable: false, maxPriority: 10});
    for(var index=1; index<=50; index++) {
        priorityValue = Math.floor((Math.random() * 10));
        // index refers to message number. Lower is the index value, earlier is the message pushed into the queue.
        message = 'Message index = ' + index + ' and Priority Value = ' + priorityValue;
        ch.sendToQueue(q, new Buffer(message), {priority: priorityValue});
        console.log(" [x] Sent '%s'", message);
    }
  });
  setTimeout(function() { conn.close(); process.exit(0) }, 500);
});
