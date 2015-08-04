<?php

//Establish connection to AMQP
$connection = new AMQPConnection();
$connection->setHost('127.0.0.1');
$connection->setLogin('guest');
$connection->setPassword('guest');
$connection->connect();

//Create and declare channel
$channel = new AMQPChannel($connection);

try {
	//Declare Exchange
	$exchange_name = 'logs';

	$exchange = new AMQPExchange($channel);
	$exchange->setType(AMQP_EX_TYPE_FANOUT);
	$exchange->setName($exchange_name);
	$exchange->declareExchange();

	//Do not declasre the queue name by setting AMQPQueue::setName()
	$queue = new AMQPQueue($channel);
	$queue->setFlags(AMQP_EXCLUSIVE);
	$queue->declareQueue();
	$queue->bind($exchange_name,$queue->getName());
	echo sprintf("Queue Name: %s", $queue->getName()), PHP_EOL;
} catch(Exception $ex) {
	print_r($ex);
}


//Read from stdin
$message = implode(' ',array_slice($argv,1));
if(empty($message)) 
	$message = "info: Hello World!";

$exchange->publish($message, '');

echo " [X] Sent {$message}", PHP_EOL;
$connection->disconnect();
