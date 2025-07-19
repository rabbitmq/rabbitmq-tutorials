<?php

declare(strict_types=1);

use Thesis\Amqp\Client;
use Thesis\Amqp\Config;
use Thesis\Amqp\Message;

require_once __DIR__.'/../vendor/autoload.php';

$client = new Client(
    new Config(
        urls: ['rabbitmq:5672'],
    ),
);

$channel = $client->channel();
$channel->exchangeDeclare(exchange: 'topics_logs', exchangeType: 'topic');

$routingKey = $argv[1] ?? 'anonymous.info';

$channel->publish(
    message: new Message(
        body: $data = implode(' ', array_slice($argv, 2)) ?: 'Hello World!',
    ),
    exchange: 'topics_logs',
    routingKey: $routingKey,
);

echo " [x] Sent {$routingKey}: {$data} \n";

$client->disconnect();
