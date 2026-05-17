#!/usr/bin/env node

const amqp = require('amqplib');

async function main() {
    const connection = await amqp.connect('amqp://rabbitmq');
    const channel = await connection.createConfirmChannel();

    const inputQueue = 'pipeline.input';

    await channel.assertQueue(inputQueue, {
        durable: true,
        arguments: { 'x-queue-type': 'quorum' }
    });

    const total = 5;
    for (let i = 1; i <= total; i++) {
        const msg = `hello #${i}`;
        channel.sendToQueue(inputQueue, Buffer.from(msg), { persistent: true });
        console.log(" [x] Sent '%s' to %s", msg, inputQueue);
    }
    await channel.waitForConfirms();

    setTimeout(function() {
        connection.close();
        process.exit(0);
    }, 500);
}

main();
