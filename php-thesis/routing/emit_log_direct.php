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

$channel->exchangeDeclare(exchange: 'direct_logs', exchangeType: 'direct');

$severity = $argv[1] ?? 'info';

$channel->publish(
    new Message(
        body: $data = implode(' ', array_slice($argv, 2)) ?: 'Hello World!',
    ),
    exchange: 'direct_logs',
    routingKey: $severity,
);

echo " [x] Sent {$severity}: {$data} \n";

$client->disconnect();
