<?php

require_once(__DIR__ . '/lib/php-amqplib/amqp.inc');
include_once(__DIR__ . '/config/config.php');

$connection = new AMQPConnection(HOST, PORT, USER, PASS, VHOST);
$channel = $connection->channel();

$channel->queue_declare('test');

echo ' [*] Waiting for messages. To exit press CTRL+C', "\n";

$callback = function($msg){
  echo " [x] Received ", $msg->body, "\n";
};

$channel->basic_consume('test', 'consumer_tag', false, true, false, false, $callback);

while(count($channel->callbacks)) {
    $channel->wait();
}

$channel->close();
$connection->close();

?>