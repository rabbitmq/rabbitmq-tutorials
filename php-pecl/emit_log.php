<?php

$connection = new AMQPConnection();
$connection->connect();

$exchange = new AMQPExchange($connection);
$exchange->declare('logs', AMQP_EX_TYPE_FANOUT);

$message = implode(' ', array_slice($argv, 1));
if(empty($message)) {
    $message = "Hello World!";
}
$exchange->publish($message, 'logs');

echo " [x] Sent $message\n";

$connection->disconnect();
