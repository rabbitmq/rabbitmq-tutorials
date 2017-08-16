<?php

// composer require enqueue/amqp-bunny
require_once __DIR__.'/vendor/autoload.php';

use Enqueue\AmqpBunny\AmqpConnectionFactory;
use Interop\Amqp\AmqpQueue;

$config = [
    'host' => 'localhost',
    'port' => 5672,
    'user' => 'guest',
    'pass' => 'guest',
    'receive_method' => 'basic_consume',
];

$connection = new AmqpConnectionFactory($config);
$context = $connection->createContext();
$context->setQos(0, 1, false);

$queue = $context->createQueue('task_queue');
$queue->addFlag(AmqpQueue::FLAG_DURABLE);

$context->declareQueue($queue);

$consumer = $context->createConsumer($queue);

echo ' [*] Waiting for messages. To exit press CTRL+C', "\n";

while (true) {
    if ($message = $consumer->receive()) {
        echo ' [x] Received ', $message->getBody(), "\n";
        sleep(substr_count($message->getBody(), '.'));
        echo ' [x] Done', "\n";
        $consumer->acknowledge($message);
    }
}

$context->close();
