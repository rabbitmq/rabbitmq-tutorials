#!/usr/bin/env node

const amqp = require('amqplib');

async function main() {
    const connection = await amqp.connect(process.env.AMQP_URL || 'amqp://localhost');
    const channel = await connection.createChannel();

    const queue = 'task_queue';

    await channel.assertQueue(queue, {
        durable: true,
        arguments: { 'x-queue-type': 'quorum' }
    });
    channel.prefetch(1);
    console.log(" [*] Waiting for messages in %s. To exit press CTRL+C", queue);
    channel.consume(queue, function(msg) {
        const secs = msg.content.toString().split('.').length - 1;

        console.log(" [x] Received %s", msg.content.toString());
        setTimeout(function() {
            console.log(" [x] Done");
            channel.ack(msg);
        }, secs * 1000);
    }, {
        // manual acknowledgment mode,
        // see /docs/confirms for details
        noAck: false
    });
}

main();
