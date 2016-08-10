<?php

$connection = new AMQPConnection();
$connection->connect();

$exchange = new AMQPExchange($connection);
$exchange->declare('topic_logs', AMQP_EX_TYPE_TOPIC);

$name = 'logworker_' . uniqid();
$queue = new AMQPQueue($connection);
$queue->declare($name);

$exchange->bind($name, 'direct_logs');

if (count($argv) > 1) {
    $bindingKeys = array_slice($argv, 1);
} else {
    echo 'Usage: ' . basename(__FILE__) . " [binding_key] ...\n";
    die(1);
}

foreach($bindingKeys as $key) {
    $exchange->bind($name, $key);
}

echo ' [*] Waiting for ' . implode($bindingKeys, ', ') . ' logs. To exit press CTRL+C', "\n";

$callback = function($message) {
    echo " [x] {$message['routing_key']}:{$message['message_body']}\n";
};

$options = array(
    'min' => 1,
    'max' => 10,
    'ack' => false
);
while ($messages = $queue->consume($options)) {
    foreach($messages as $message) {
        $callback($message);
    }
}
