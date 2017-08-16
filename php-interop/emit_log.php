<?php

// composer require enqueue/amqp-bunny
require_once __DIR__.'/vendor/autoload.php';

use Enqueue\AmqpBunny\AmqpConnectionFactory;
use Interop\Amqp\AmqpTopic;

$config = [
    'host' => 'localhost',
    'port' => 5672,
    'user' => 'guest',
    'pass' => 'guest',
];

$connection = new AmqpConnectionFactory($config);
$context = $connection->createContext();

$topic = $context->createTopic('logs');
$topic->setType(AmqpTopic::TYPE_FANOUT);

$context->declareTopic($topic);

$data = implode(' ', array_slice($argv, 1));
if (empty($data)) {
    $data = 'info: Hello World!';
}
$message = $context->createMessage($data);

$context->createProducer()->send($topic, $message);

echo ' [x] Sent ', $data, "\n";

$context->close();
