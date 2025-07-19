<?php

declare(strict_types=1);

use Thesis\Amqp\Client;
use Thesis\Amqp\Config;
use Thesis\Amqp\DeliveryMessage;
use Thesis\Amqp\Message;
use function Amp\trapSignal;

require_once __DIR__.'/../vendor/autoload.php';

function fib(int $n): int
{
    if ($n === 0) {
        return 0;
    }

    if ($n === 1) {
        return 1;
    }

    return fib($n - 1) + fib($n - 2);
}

$client = new Client(
    new Config(
        urls: ['rabbitmq:5672'],
    ),
);

$channel = $client->channel();
$channel->queueDeclare('rpc_queue');

echo " [x] Awaiting RPC requests\n";

$channel->qos(prefetchCount: 1);
$channel->consume(
    callback: static function (DeliveryMessage $delivery): void {
        $n = intval($delivery->message->body);
        echo " [.] fib({$n})\n";

        $delivery->reply(
            new Message((string) fib($n)),
        );
    },
    queue: 'rpc_queue',
    noAck: true,
);

trapSignal([\SIGTERM, \SIGINT]);

$client->disconnect();
