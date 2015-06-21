#!/usr/bin/env php
<?php
/**
	@author Chimdi Azubuike <me@chimdi.com>
*/

//Establish connection to AMQP
$connection = new AMQPConnection();
$connection->setHost('127.0.0.1');
$connection->setLogin('guest');
$connection->setPassword('guest');
$connection->connect();

//Create and declare channel
$channel = new AMQPChannel($connection);


//AMQPC Exchange is the publishing mechanism
$exchange = new AMQPExchange($channel);


try{
	$routing_key = 'hello';

	$queue = new AMQPQueue($channel);
	$queue->setName($routing_key);
	$queue->setFlags(AMQP_NOPARAM);
	$queue->declareQueue();


	$message = 'howdy-do';
	$exchange->publish($message, $routing_key);

	$connection->disconnect();
}catch(Exception $ex){
	print_r($ex);
}
