#!/usr/bin/env node

const amqp = require('amqplib');

async function main() {
    const connection = await amqp.connect(process.env.AMQP_URL || 'amqp://localhost');
    const channel = await connection.createChannel();

    const queue = 'task_queue';
    const msg = process.argv.slice(2).join(' ') || "Hello World!";

    await channel.assertQueue(queue, {
        durable: true,
        arguments: { 'x-queue-type': 'quorum' }
    });
    channel.sendToQueue(queue, Buffer.from(msg), {
        persistent: true
    });
    console.log(" [x] Sent '%s'", msg);

    setTimeout(function() {
        connection.close();
        process.exit(0);
    }, 500);
}

main();
