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

$channel->exchangeDeclare(exchange: 'topics_logs', exchangeType: 'topic');
$queue = $channel->queueDeclare(exclusive: true);

$bindingKeys = array_slice($argv, 1);
if ($bindingKeys === []) {
    file_put_contents('php://stderr', "Usage: $argv[0] [binding_key]\n");
    exit(1);
}

foreach ($bindingKeys as $bindingKey) {
    $channel->queueBind(queue: $queue->name, exchange: 'topics_logs', routingKey: $bindingKey);
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
