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
$channel->queueDeclare('hello');

echo " [*] Waiting for messages. To exit press CTRL+C\n";

$channel->consume(
    static function (DeliveryMessage $delivery): void {
        echo " [x] Received {$delivery->message->body} \n";
    },
    queue: 'hello',
    noAck: true,
);

trapSignal([\SIGTERM, \SIGINT]);

$client->disconnect();
