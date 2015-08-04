<?php


//Establish connection AMQP
$connection = new AMQPConnection();
$connection->setHost('127.0.0.1');
$connection->setLogin('guest');
$connection->setPassword('guest');
$connection->connect();

//Create and declare channel
$channel = new AMQPChannel($connection);

$routing_key = 'task_queue';

$callback_func = function(AMQPEnvelope $message, AMQPQueue $q) use (&$max_jobs) {
	echo " [x] Received: ", $message->getBody(), PHP_EOL;
	sleep(1);
	echo " [X] Done", PHP_EOL;
	$q->ack($message->getDeliveryTag());
};

try{
	$queue = new AMQPQueue($channel);
	$queue->setName($routing_key);
	$queue->setFlags(AMQP_DURABLE);
	$queue->declareQueue();


	echo ' [*] Waiting for logs. To exit press CTRL+C', PHP_EOL;
	$queue->consume($callback_func);
}catch(AMQPQueueException $ex){
	print_r($ex);
}catch(Exception $ex){
	print_r($ex);
}

$connection->disconnect();
