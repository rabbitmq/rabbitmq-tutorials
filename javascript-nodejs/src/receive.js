#!/usr/bin/env node

const amqp = require('amqplib');

async function main() {
    const connection = await amqp.connect(process.env.AMQP_URL || 'amqp://localhost');
    const channel = await connection.createChannel();

    const queue = 'hello';

    await channel.assertQueue(queue, {
        durable: true,
        arguments: { 'x-queue-type': 'quorum' }
    });

    console.log(" [*] Waiting for messages in %s. To exit press CTRL+C", queue);

    channel.consume(queue, function(msg) {
        console.log(" [x] Received %s", msg.content.toString());
    }, {
        noAck: true
    });
}

main();
