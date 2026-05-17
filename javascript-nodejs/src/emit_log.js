#!/usr/bin/env node

const amqp = require('amqplib');

async function main() {
    const connection = await amqp.connect(process.env.AMQP_URL || 'amqp://localhost');
    const channel = await connection.createChannel();

    const exchange = 'logs';
    const msg = process.argv.slice(2).join(' ') || 'Hello World!';

    await channel.assertExchange(exchange, 'fanout', {
        durable: false
    });
    channel.publish(exchange, '', Buffer.from(msg));
    console.log(" [x] Sent %s", msg);

    setTimeout(function() {
        connection.close();
        process.exit(0);
    }, 500);
}

main();
