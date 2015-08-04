#!/usr/bin/env node

var amqp = require('amqplib');

var conn = amqp.connect('amqp://localhost');
conn.then(createChannel).then(null, console.warn);

function createChannel(conn) {
  process.once('SIGINT', function() { conn.close(); });
  return conn.createChannel().then(consume);
}

function consume(ch) {
  var ok = ch.assertQueue('rpc_queue', {durable: false});
  ok = ok.then(function() {
    ch.prefetch(1);
    return ch.consume('rpc_queue', reply);
  });

  return ok.then(function(_ignore) {
    console.log(' [x] Awaiting RPC requests');
  });

  function reply(msg) {
    var n = parseInt(msg.content.toString());
    console.log(' [.] fib(%d)', n);
    var response = fib(n);
    ch.sendToQueue( msg.properties.replyTo,
        new Buffer(response.toString()),
        {correlationId: msg.properties.correlationId});
    ch.ack(msg);
  }
}

function fib(n) {
  if(n == 0)
    return 0;
  else if(n == 1)
    return 1;
  else
    return fib(n-1) + fib(n-2);
}
