<?php

require_once(__DIR__ . '/lib/php-amqplib/amqp.inc');
include_once(__DIR__ . '/config/config.php');

$connection = new AMQPConnection(HOST, PORT, USER, PASS, VHOST);
$channel = $connection->channel();
$channel->exchange_declare('logs', 'fanout');

list($queue_name, ,) = $channel->queue_declare();

$channel->queue_bind($queue_name, 'logs');

echo ' [*] Waiting for logs. To exit press CTRL+C', "\n";

$callback = function($msg){
  echo $msg->body, "\n";
};

$channel->basic_consume($queue_name, 'consumer_tag', false, true, false, false, $callback);
  
while(count($channel->callbacks)) {
    $channel->wait();
}

$channel->close();
$connection->close();

?>