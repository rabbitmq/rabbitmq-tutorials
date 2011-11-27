<?php
// TODO qos
$connection = new AMQPConnection();
$connection->connect();

$queue = new AMQPQueue($connection);
$queue->declare('task_queue', AMQP_DURABLE);

echo ' [*] Waiting for messages. To exit press CTRL+C', "\n";

$callback = function($message) use ($queue) {
    echo " [x] Received ", $message['message_body'], "\n";
    sleep(substr_count($message['message_body'], '.'));
    echo " [x] Done", "\n";
    $queue->ack($message['delivery_tag']);
};

$options = array(
    'min' => 1,
    'max' => 1,
    'ack' => false
);
while ($messages = $queue->consume($options)) {
    foreach($messages as $message) {
        $callback($message);
    }
}
