<?php

$connection = new AMQPConnection();
$connection->connect();

$exchange = new AMQPExchange($connection);
$exchange->declare('topic_logs', AMQP_EX_TYPE_TOPIC);

if (count($argv) > 2) {
    $routingKey = $argv[1];
    unset($argv[1]);
} else {
    $routingKey = 'anonymous.info';
}
$message = implode(' ', array_slice($argv, 1));
if(empty($message)) {
    $message = "Hello World!";
}
$exchange->publish($message, $routingKey);

echo " [x] Sent $routingKey:$message\n";

$connection->disconnect();
