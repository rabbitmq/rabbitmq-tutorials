<?php

//Establish connection to AMQP
$connection = new AMQPConnection();
$connection->setHost('127.0.0.1');
$connection->setLogin('guest');
$connection->setPassword('guest');
$connection->connect();


//Declare Channel
$channel = new AMQPChannel($connection);


$routing_key = isset($argv[1]) && !empty($argv[1]) ? $argv[1] : 'anonymous.info';

$message = implode(' ',array_slice($argv, 2));
if(empty($message)) $message = "Hello World!";


try {
	//Declare Exchange
	$exchange_name = 'topic_logs';
	$exchange = new AMQPExchange($channel);
	$exchange->setType(AMQP_EX_TYPE_TOPIC);
	$exchange->setName($exchange_name);
	$exchange->declareExchange();

	$exchange->publish($message, $routing_key);
	echo " [x] Sent {$routing_key}:{$message}", PHP_EOL;
} catch(Exception $ex) {
	print_r($ex);
}
