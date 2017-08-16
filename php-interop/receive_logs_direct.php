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

$topic = $context->createTopic('direct_logs');
$topic->setType(AmqpTopic::TYPE_DIRECT);

$context->declareTopic($topic);

$queue = $context->createTemporaryQueue();

$severities = array_slice($argv, 1);
if (empty($severities)) {
    file_put_contents('php://stderr', "Usage: $argv[0] [info] [warning] [error]\n");
    exit(1);
}

foreach ($severities as $severity) {
    $context->bind(new AmqpBind($topic, $queue, $severity));
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
