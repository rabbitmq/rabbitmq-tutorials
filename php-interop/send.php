<?php

// composer require enqueue/amqp-bunny
require_once __DIR__.'/vendor/autoload.php';

use Enqueue\AmqpBunny\AmqpConnectionFactory;

$config = [
    'host' => 'localhost',
    'port' => 5672,
    'user' => 'guest',
    'pass' => 'guest',
];

$connection = new AmqpConnectionFactory($config);
$context = $connection->createContext();

$queue = $context->createQueue('hello');
$context->declareQueue($queue);

$message = $context->createMessage('Hello World!');

$context->createProducer()->send($queue, $message);

echo " [x] Sent 'Hello World!'\n";

$context->close();
