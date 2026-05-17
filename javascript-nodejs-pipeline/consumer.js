#!/usr/bin/env node

const amqp = require('amqplib');

async function main() {
    const connection = await amqp.connect('amqp://localhost');
    const channel = await connection.createChannel();

    const outputQueue = 'pipeline.output';

    await channel.assertQueue(outputQueue, {
        durable: true,
        arguments: { 'x-queue-type': 'quorum' }
    });

    console.log(" [*] Waiting for messages in %s. To exit press CTRL+C", outputQueue);

    channel.consume(outputQueue, function(msg) {
        console.log(" [x] Received '%s'", msg.content.toString());
    }, {
        noAck: true
    });
}

main();
