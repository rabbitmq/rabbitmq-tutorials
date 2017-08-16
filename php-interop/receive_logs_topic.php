<?php

// composer require enqueue/amqp-bunny
require_once __DIR__.'/vendor/autoload.php';

use Enqueue\AmqpBunny\AmqpConnectionFactory;
use Interop\Amqp\AmqpConsumer;
use Interop\Amqp\AmqpTopic;
use Interop\Amqp\Impl\AmqpBind;

$config = [
    'host' => 'localhost',
    'port' => 5672,
    'user' => 'guest',
    'pass' => 'guest',
    'receive_method' => 'basic_consume',
];

$connection = new AmqpConnectionFactory($config);
$context = $connection->createContext();

$topic = $context->createTopic('topic_logs');
$topic->setType(AmqpTopic::TYPE_TOPIC);

$context->declareTopic($topic);

$queue = $context->createTemporaryQueue();

$binding_keys = array_slice($argv, 1);
if (empty($binding_keys)) {
    file_put_contents('php://stderr', "Usage: $argv[0] [binding_key]\n");
    exit(1);
}

foreach ($binding_keys as $binding_key) {
    $context->bind(new AmqpBind($topic, $queue, $binding_key));
}

$consumer = $context->createConsumer($queue);
$consumer->addFlag(AmqpConsumer::FLAG_NOACK);

echo ' [*] Waiting for logs. To exit press CTRL+C', "\n";

while (true) {
    if ($message = $consumer->receive()) {
        echo ' [x] '.$message->getRoutingKey().':'.$message->getBody()."\n";
    }
}

$context->close();
