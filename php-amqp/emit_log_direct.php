<?php

//Establish connection to AMQP
$connection = new AMQPConnection();
$connection->setHost('127.0.0.1');
$connection->setLogin('guest');
$connection->setPassword('guest');
$connection->connect();


//Declare Channel
$channel = new AMQPChannel($connection);


$routing_key = $severity = $argv[1];
if(empty($severity)) $severity = 'info';

$message = implode(' ',array_slice($argv, 2));
if(empty($message)) $message = 'Hello World!';

try {
	//Declare Exchange
	$exchange_name = 'direct_logs';
	$exchange = new AMQPExchange($channel);
	$exchange->setType(AMQP_EX_TYPE_DIRECT);
	$exchange->setName($exchange_name);
	$exchange->declareExchange();

	$queue = new AMQPQueue($channel);
	$queue->setFlags(AMQP_EXCLUSIVE);
	$queue->setName('monitor.1');
	$queue->declareQueue();

	$queue->bind($exchange_name, $routing_key);
	$exchange->publish($message,$routing_key);
	echo " [x] Sent {$severity}:{$message}", PHP_EOL;
} catch(Exception $ex) {
	print_r($ex);
}

$connection->disconnect();
