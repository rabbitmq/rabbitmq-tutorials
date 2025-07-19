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
$channel->exchangeDeclare(exchange: 'logs', exchangeType: 'fanout');

$channel->publish(
    new Message(
        body: $data = implode(' ', array_slice($argv, 1)) ?: 'info: Hello World!',
    ),
    exchange: 'logs',
);

$client->disconnect();
