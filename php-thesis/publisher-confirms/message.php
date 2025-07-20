<?php

declare(strict_types=1);

use Thesis\Amqp\Client;
use Thesis\Amqp\Config;
use Thesis\Amqp\Message;

require __DIR__ . '/../vendor/autoload.php';

$client = new Client(
    new Config(
        urls: ['rabbitmq:5672'],
    ),
);

$channel = $client->channel();
$channel->confirmSelect();

$queue = $channel->queueDeclare(exclusive: true);

$channel
    ->publish(
        new Message('Hello World!'),
        routingKey: $queue->name,
    )
    ?->await();

echo "Message sent\n";

$client->disconnect();
