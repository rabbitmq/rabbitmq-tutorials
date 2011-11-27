<?php
$connection = new AMQPConnection();
$connection->connect();

$exchange = new AMQPExchange($connection, '');

$queue = new AMQPQueue($connection);
$queue->declare('hello');

$exchange->publish('Hello World!', 'hello');

echo " [x] Sent 'Hello World!'\n";

$connection->disconnect();
