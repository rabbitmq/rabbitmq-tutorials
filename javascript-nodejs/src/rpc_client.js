#!/usr/bin/env node

var amqp = require('amqplib');
var when = require('when');

var conn = amqp.connect('amqp://localhost')
conn.then(createChannel).then(null, console.warn);

function createChannel(conn) {
  return when(conn.createChannel().then(requestFib)).ensure(function() { conn.close(); });
}

function requestFib(ch) {
  var answer = when.defer();
  var correlationId = generateUuid();

  function maybeAnswer(msg) {
    if (msg.properties.correlationId === correlationId) {
      answer.resolve(msg.content.toString());
    }
  }

  var ok = ch.assertQueue('', {exclusive: true})
    .then(function(qok) { return qok.queue; });

  ok = ok.then(function(queue) {
    return ch.consume(queue, maybeAnswer, {noAck: true})
      .then(function() { return queue; });
  });

  ok = ok.then(function(queue) {
    console.log(' [x] Requesting fib(30)');
    ch.sendToQueue('rpc_queue', new Buffer('30'), {
      correlationId: correlationId, replyTo: queue
    });
    return answer.promise;
  });

  return ok.then(function(fibN) {
    console.log(' [.] Got %d', fibN);
  });
}

function generateUuid() {
  return Math.random().toString() + Math.random().toString() + Math.random().toString();
}
