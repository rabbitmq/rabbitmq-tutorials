<?php

declare(strict_types=1);

use Thesis\Amqp\Client;
use Thesis\Amqp\Config;
use Thesis\Amqp\DeliveryMessage;
use function Amp\trapSignal;

require_once __DIR__.'/../vendor/autoload.php';

$client = new Client(
    new Config(
        urls: ['rabbitmq:5672'],
    ),
);

$channel = $client->channel();

$channel->exchangeDeclare(exchange: 'logs', exchangeType: 'fanout');
$queue = $channel->queueDeclare(exclusive: true);
$channel->queueBind(queue: $queue->name, exchange: 'logs');

echo " [*] Waiting for logs. To exit press CTRL+C\n";

$channel->consume(
    callback: static function (DeliveryMessage $delivery): void {
        echo " [x] {$delivery->message->body} \n";
    },
    queue: $queue->name,
    noAck: true,
);

trapSignal([\SIGTERM, \SIGINT]);

$client->disconnect();
