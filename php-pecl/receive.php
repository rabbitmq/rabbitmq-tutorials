<?php

$connection = new AMQPConnection();
$connection->connect();

$queue = new AMQPQueue($connection);
$queue->declare('hello');

echo ' [*] Waiting for messages. To exit press CTRL+C', "\n";

$options = array(
    'min' => 1,
    'max' => 10,
    'ack' => true
);
while ($messages = $queue->consume($options)) {
    foreach($messages as $message) {
        echo " [x] Received {$message['message_body']}\n";
    }
}
