#!/usr/bin/env node

// A consumer-and-producer: reads from pipeline.input, transforms
// each message, republishes it to pipeline.output, and only then
// acknowledges the input message.

const amqp = require('amqplib');

async function main() {
    const connection = await amqp.connect('amqp://localhost');
    const channel = await connection.createChannel();

    const inputQueue = 'pipeline.input';
    const outputQueue = 'pipeline.output';

    await channel.assertQueue(inputQueue, {
        durable: true,
        arguments: { 'x-queue-type': 'quorum' }
    });
    await channel.assertQueue(outputQueue, {
        durable: true,
        arguments: { 'x-queue-type': 'quorum' }
    });

    channel.prefetch(1);
    console.log(" [*] Waiting for messages in %s. To exit press CTRL+C", inputQueue);

    channel.consume(inputQueue, function(msg) {
        const body = msg.content.toString();
        const transformed = body.toUpperCase();

        console.log(" [x] Received '%s', forwarding '%s' to %s", body, transformed, outputQueue);
        channel.sendToQueue(outputQueue, Buffer.from(transformed), { persistent: true });
        channel.ack(msg);
    }, {
        noAck: false
    });
}

main();
