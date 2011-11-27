<?php
$connection = new AMQPConnection();
$connection->connect();

$exchange = new AMQPExchange($connection);
$exchange->declare('logs', AMQP_EX_TYPE_FANOUT);

$name = 'logworker_' . uniqid();
$queue = new AMQPQueue($connection);
$queue->declare($name);

$exchange->bind($name, 'logs');

echo ' [*] Waiting for logs. To exit press CTRL+C', "\n";

$options = array(
    'min' => 1,
    'max' => 10,
    'ack' => false
);
while ($messages = $queue->consume($options)) {
    foreach($messages as $message) {
        echo " [x] Received {$message['message_body']}\n";
    }
}
