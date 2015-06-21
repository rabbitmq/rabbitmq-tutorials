#!/usr/bin/env php
<?php
/**
	@author Chimdi Azubuike <me@chimdi.com>
*/

//Establish connection AMQP
$connection = new AMQPConnection();
$connection->setHost('127.0.0.1');
$connection->setLogin('guest');
$connection->setPassword('guest');
$connection->connect();

//Create and declare channel
$channel = new AMQPChannel($connection);

//AMQPC Exchange is the publishing mechanism
$exchange = new AMQPExchange($channel);


$callback_func = function(AMQPEnvelope $message, AMQPQueue $q) use (&$max_consume) {
	echo PHP_EOL, "------------", PHP_EOL;
	echo " [x] Received ", $message->getBody(), PHP_EOL;
	echo PHP_EOL, "------------", PHP_EOL;

	$q->nack($message->getDeliveryTag());
	sleep(1);
};

try{
	$routing_key = 'hello';
	
	$queue = new AMQPQueue($channel);
	$queue->setName($routing_key);
	$queue->setFlags(AMQP_NOPARAM);
	$queue->declareQueue();

	echo ' [*] Waiting for messages. To exit press CTRL+C ', PHP_EOL;
	$queue->consume($callback_func);
}catch(AMQPQueueException $ex){
	print_r($ex);
}catch(Exception $ex){
	print_r($ex);
}

echo 'Close connection...', PHP_EOL;
$queue->cancel();
$connection->disconnect();

