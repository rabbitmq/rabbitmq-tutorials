<?php

require_once(__DIR__ . '/lib/php-amqplib/amqp.inc');

$connection = new AMQPConnection('localhost', 5672, 'guest', 'guest');
$channel = $connection->channel();


$channel->queue_declare('test', false, false, false, false);

$msg = new AMQPMessage('Hello World!');
$channel->basic_publish($msg, '', 'test');

echo " [x] Sent 'Hello World!'\n";

$channel->close();
$connection->close();

?>