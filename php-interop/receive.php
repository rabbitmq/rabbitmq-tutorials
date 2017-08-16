<?php

// composer require enqueue/amqp-bunny
require_once __DIR__.'/vendor/autoload.php';

use Enqueue\AmqpBunny\AmqpConnectionFactory;
use Interop\Amqp\AmqpConsumer;

$config = [
    'host' => 'localhost',
    'port' => 5672,
    'user' => 'guest',
    'pass' => 'guest',
    'receive_method' => 'basic_consume',
];

$connection = new AmqpConnectionFactory($config);
$context = $connection->createContext();

$queue = $context->createQueue('hello');
$context->declareQueue($queue);

$consumer = $context->createConsumer($queue);
$consumer->addFlag(AmqpConsumer::FLAG_NOACK);

echo ' [*] Waiting for messages. To exit press CTRL+C', "\n";

while (true) {
    if ($message = $consumer->receive()) {
        echo ' [x] Received ', $message->getBody(), "\n";
    }
}

$context->close();
