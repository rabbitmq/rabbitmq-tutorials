#!/usr/bin/env node

const amqp = require('amqplib');

const args = process.argv.slice(2);

if (args.length === 0) {
    console.log("Usage: rpc_client.js num");
    process.exit(1);
}

async function main() {
    const connection = await amqp.connect('amqp://localhost');
    const channel = await connection.createChannel();

    const correlationId = generateUuid();
    const num = parseInt(args[0]);

    console.log(' [x] Requesting fib(%d)', num);

    // Consume from the Direct Reply-to pseudo-queue (noAck is mandatory)
    channel.consume('amq.rabbitmq.reply-to', function(msg) {
        if (msg.properties.correlationId === correlationId) {
            console.log(' [.] Got %s', msg.content.toString());
            setTimeout(function() {
                connection.close();
                process.exit(0);
            }, 500);
        }
    }, {
        noAck: true
    });

    channel.sendToQueue('rpc_queue',
        Buffer.from(num.toString()), {
            correlationId: correlationId,
            replyTo: 'amq.rabbitmq.reply-to'
        });
}

function generateUuid() {
    return Math.random().toString() +
        Math.random().toString() +
        Math.random().toString();
}

main();
