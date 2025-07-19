<?php

declare(strict_types=1);

require_once __DIR__.'/../vendor/autoload.php';

use Thesis\Amqp\Client;
use Thesis\Amqp\Config;
use Thesis\Amqp\DeliveryMessage;
use function Amp\delay;
use function Amp\trapSignal;

$client = new Client(
    new Config(
        urls: ['rabbitmq:5672'],
    ),
);

$channel = $client->channel();
$channel->queueDeclare('task_queue', durable: true);

echo " [*] Waiting for messages. To exit press CTRL+C\n";

$channel->qos(prefetchCount: 1);
$channel->consume(
    callback: static function (DeliveryMessage $delivery): void {
        echo " [x] Received {$delivery->message->body} \n";
        delay(substr_count($delivery->message->body, '.'));
        echo " [x] Done\n";
        $delivery->ack();
    },
    queue: 'task_queue',
);

trapSignal([\SIGTERM, \SIGINT]);

$client->disconnect();
