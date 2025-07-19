<?php

declare(strict_types=1);

use Thesis\Amqp\Client;
use Thesis\Amqp\Config;
use Thesis\Amqp\Message;
use Thesis\Amqp\PublishMessage;

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
    ->publishBatch(array_map(
        static fn(int $number): PublishMessage => new PublishMessage(new Message("{$number}"), routingKey: $queue->name),
        range(1, 100),
    ))
    ->await()
    ->ensureAllPublished()
;

echo "Messages sent\n";

$client->disconnect();
