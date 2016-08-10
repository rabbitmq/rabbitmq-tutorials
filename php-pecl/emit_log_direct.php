<?php

$connection = new AMQPConnection();
$connection->connect();

$exchange = new AMQPExchange($connection);
$exchange->declare('direct_logs');

if (count($argv) > 2) {
    $severity = $argv[1];
    unset($argv[1]);
} else {
    $severity = 'info';
}
$message = implode(' ', array_slice($argv, 1));
if(empty($message)) {
    $message = "Hello World!";
}
$exchange->publish($message, $severity);

echo " [x] Sent $severity:$message\n";

$connection->disconnect();
