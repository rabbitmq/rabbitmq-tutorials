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

$channel->exchangeDeclare(exchange: 'direct_logs', exchangeType: 'direct');
$queue = $channel->queueDeclare(exclusive: true);

$severities = array_slice($argv, 1);
if ($severities === []) {
    file_put_contents('php://stderr', "Usage: $argv[0] [info] [warning] [error]\n");
    exit(1);
}

foreach ($severities as $severity) {
    $channel->queueBind(queue: $queue->name, exchange: 'direct_logs', routingKey: $severity);
}

echo " [*] Waiting for logs. To exit press CTRL+C\n";

$channel->consume(
    callback: static function (DeliveryMessage $delivery): void {
        echo " [x] {$delivery->routingKey}: {$delivery->message->body} \n";
    },
    queue: $queue->name,
    noAck: true,
);

trapSignal([\SIGTERM, \SIGINT]);

$client->disconnect();
