<?php

// composer require enqueue/amqp-bunny
require_once __DIR__.'/vendor/autoload.php';

use Enqueue\AmqpBunny\AmqpConnectionFactory;

$config = [
    'host' => 'localhost',
    'port' => 5672,
    'user' => 'guest',
    'pass' => 'guest',
    'receive_method' => 'basic_consume',
];

function fib($n)
{
    $a = 0;
    $b = 1;

    for ($i = 0; $i < $n; $i++) {
        [$a, $b] = [$b, $a + $b];
    }

    return $a;
}

$connection = new AmqpConnectionFactory($config);
$context = $connection->createContext();
$context->setQos(0, 1, false);

$rpc_queue = $context->createQueue('rpc_queue');
$context->declareQueue($rpc_queue);

$consumer = $context->createConsumer($rpc_queue);

echo " [x] Awaiting RPC requests\n";

while (true) {
    if ($req = $consumer->receive()) {
        $n = (int) ($req->getBody());
        echo ' [.] fib(', $n, ")\n";

        $msg = $context->createMessage((string) fib($n));
        $msg->setCorrelationId($req->getCorrelationId());

        $reply_queue = $context->createQueue($req->getReplyTo());
        $context->createProducer()->send($reply_queue, $msg);

        $consumer->acknowledge($req);
    }
}

$context->close();
