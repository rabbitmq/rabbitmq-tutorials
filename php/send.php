<?php

require_once(__DIR__ . '/lib/php-amqplib/amqp.inc');
include_once(__DIR__ . '/config/config.php');

$connection = new AMQPConnection(HOST, PORT, USER, PASS, VHOST);
$channel = $connection->channel();

$channel->queue_declare('test');

$msg = new AMQPMessage('Hello World!');

$channel->basic_publish($msg, '', 'test');

echo " [x] Sent 'Hello World!'\n";

$channel->close();
$connection->close();

?>