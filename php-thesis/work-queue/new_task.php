<?php

declare(strict_types=1);

require_once __DIR__.'/../vendor/autoload.php';

use Thesis\Amqp\Client;
use Thesis\Amqp\Config;
use Thesis\Amqp\DeliveryMode;
use Thesis\Amqp\Message;

$client = new Client(
    new Config(
        urls: ['rabbitmq:5672'],
    ),
);

$channel = $client->channel();
$channel->queueDeclare('task_queue', durable: true);

$channel->publish(
    new Message(
        body: $data = implode(' ', array_slice($argv, 1)) ?: 'Hello World!',
        deliveryMode: DeliveryMode::Persistent,
    ),
    routingKey: 'task_queue',
);

echo " [x] Sent {$data} \n";
