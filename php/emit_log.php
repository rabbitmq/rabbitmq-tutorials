<?php

require_once(__DIR__ . '/lib/php-amqplib/amqp.inc');
include_once(__DIR__ . '/config/config.php');

$connection = new AMQPConnection(HOST, PORT, USER, PASS, VHOST);
$channel = $connection->channel();
$channel->exchange_declare('logs', 'fanout');

$data = implode(' ', array_slice($argv, 1));
if(empty($data)) $data = "info: Hello World!";
$msg = new AMQPMessage($data);

$channel->basic_publish($msg, 'logs');

echo " [x] Sent ", $data, "\n";

$channel->close();
$connection->close();

?>