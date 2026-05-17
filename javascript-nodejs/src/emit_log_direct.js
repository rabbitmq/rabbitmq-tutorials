#!/usr/bin/env node

const amqp = require('amqplib');

async function main() {
    const connection = await amqp.connect(process.env.AMQP_URL || 'amqp://localhost');
    const channel = await connection.createChannel();

    const exchange = 'direct_logs';
    const args = process.argv.slice(2);
    const msg = args.slice(1).join(' ') || 'Hello World!';
    const severity = (args.length > 0) ? args[0] : 'info';

    await channel.assertExchange(exchange, 'direct', {
        durable: false
    });
    channel.publish(exchange, severity, Buffer.from(msg));
    console.log(" [x] Sent %s: '%s'", severity, msg);

    setTimeout(function() {
        connection.close();
        process.exit(0);
    }, 500);
}

main();
