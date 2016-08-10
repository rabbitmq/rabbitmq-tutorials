<?php
$connection = new AMQPConnection();
$connection->connect();

$exchange = new AMQPExchange($connection, '');

$queue = new AMQPQueue($connection);
$queue->declare('task_queue', AMQP_DURABLE);

$message = implode(' ', array_slice($argv, 1));
if(empty($message)) {
    $message = "Hello World!";
}

$exchange->publish(
    $message, 
    'task_queue', 
    0, 
    array(
        'delivery_mode' => 2 # make message persistent
    )
);

echo " [x] Sent $message\n";

$connection->disconnect();
