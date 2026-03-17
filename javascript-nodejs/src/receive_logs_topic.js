#!/usr/bin/env node

const amqp = require('amqplib');

const args = process.argv.slice(2);

if (args.length == 0) {
    console.log("Usage: receive_logs_topic.js <facility>.<severity>");
    process.exit(1);
}

async function main() {
    const connection = await amqp.connect('amqp://localhost');
    const channel = await connection.createChannel();

    const exchange = 'topic_logs';

    await channel.assertExchange(exchange, 'topic', {
        durable: false
    });

    const q = await channel.assertQueue('', {
        exclusive: true
    });
    console.log(' [*] Waiting for logs. To exit press CTRL+C');

    args.forEach(function(key) {
        channel.bindQueue(q.queue, exchange, key);
    });

    channel.consume(q.queue, function(msg) {
        console.log(" [x] %s:'%s'", msg.fields.routingKey, msg.content.toString());
    }, {
        noAck: true
    });
}

main();
