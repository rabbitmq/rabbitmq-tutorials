<?php

// composer require enqueue/amqp-bunny
require_once __DIR__.'/vendor/autoload.php';

use Enqueue\AmqpBunny\AmqpConnectionFactory;
use Interop\Amqp\AmqpMessage;
use Interop\Amqp\AmqpQueue;

$config = [
    'host' => 'localhost',
    'port' => 5672,
    'user' => 'guest',
    'pass' => 'guest',
];

$connection = new AmqpConnectionFactory($config);
$context = $connection->createContext();

$queue = $context->createQueue('task_queue');
$queue->addFlag(AmqpQueue::FLAG_DURABLE);

$context->declareQueue($queue);

$data = implode(' ', array_slice($argv, 1));
if (empty($data)) {
    $data = 'Hello World!';
}
$message = $context->createMessage($data);
$message->setDeliveryMode(AmqpMessage::DELIVERY_MODE_PERSISTENT);

$context->createProducer()->send($queue, $message);

echo ' [x] Sent ', $data, "\n";

$context->close();
