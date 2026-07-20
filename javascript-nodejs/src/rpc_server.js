#!/usr/bin/env node

const amqp = require('amqplib');

async function main() {
    const connection = await amqp.connect('amqp://localhost');
    const channel = await connection.createChannel();

    const queue = 'rpc_queue';

    await channel.assertQueue(queue, {
        durable: true,
        arguments: { 'x-queue-type': 'quorum' }
    });
    channel.prefetch(1);
    console.log(' [x] Awaiting RPC requests');
    channel.consume(queue, function reply(msg) {
        const n = parseInt(msg.content.toString());

        console.log(" [.] fib(%d)", n);

        const r = fibonacci(n);

        channel.sendToQueue(msg.properties.replyTo,
            Buffer.from(r.toString()), {
                correlationId: msg.properties.correlationId
            });

        channel.ack(msg);
    });
}

function fibonacci(n) {
    let a = 0;
    let b = 1;
    for (let i = 0; i < n; i++) {
        [a, b] = [b, a + b];
    }
    return a;
}

main();
