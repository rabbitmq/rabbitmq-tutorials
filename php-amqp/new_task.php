<?php

//Establish connection to AMQP
$connection = new AMQPConnection();
$connection->setHost('127.0.0.1');
$connection->setLogin('guest');
$connection->setPassword('guest');
$connection->connect();



//Create and declare channel
$channel = new AMQPChannel($connection);
$channel->setPrefetchCount(1);


$routing_key = 'task_queue';

try{
	$queue = new AMQPQueue($channel);
	$queue->setName($routing_key);
	$queue->setFlags(AMQP_DURABLE);
	$queue->declareQueue();

}catch(Exception $ex){
	print_r($ex);
}


//Read from stdin
$message = implode(' ', array_slice($argv,1));
if(empty($message)) 
	$message = "Hello World!";

$exchange = new AMQPExchange($channel);
$exchange->publish($message, $routing_key);

echo " [x] Sent {$data}", PHP_EOL;

$connection->disconnect();
