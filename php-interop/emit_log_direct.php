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

$topic = $context->createTopic('direct_logs');
$topic->setType(AmqpTopic::TYPE_DIRECT);

$context->declareTopic($topic);

$severity = isset($argv[1]) && !empty($argv[1]) ? $argv[1] : 'info';

$data = implode(' ', array_slice($argv, 2));
if (empty($data)) {
    $data = 'Hello World!';
}

$message = $context->createMessage($data);
$message->setRoutingKey($severity);

$context->createProducer()->send($topic, $message);

echo ' [x] Sent ',$severity,':',$data," \n";

$context->close();
