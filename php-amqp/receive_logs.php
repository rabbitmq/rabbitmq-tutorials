<?php


//Establish connection to AMQP
$connection = new AMQPConnection();
$connection->setHost('127.0.0.1');
$connection->setLogin('guest');
$connection->setPassword('guest');
$connection->connect();

//setup channel connection
$channel = new AMQPChannel($connection);


$callback_func = function(AMQPEnvelope $message, AMQPQueue $q) use (&$max_jobs) { 
	echo " [x] Received: ", $message->getBody(), PHP_EOL;
	sleep(1);
	$q->nack($message->getDeliveryTag());
};


try {
	//Declare Exchange
	$exchange_name = 'logs';
	$exchange = new AMQPExchange($channel);
	$exchange->setType(AMQP_EX_TYPE_FANOUT);
	$exchange->setName($exchange_name);
	$exchange->declareExchange();


	$queue = new AMQPQueue($channel);
	$queue->setFlags(AMQP_EXCLUSIVE); //allow server to define name
	$queue->declareQueue();
	$queue->bind($exchange_name,$queue->getName());

	echo ' [*] Waiting for logs. To exit press CTRL+C', PHP_EOL;
	$queue->consume($callback_func);

} catch(AMQPQueueException $ex) {
	print_r($ex);
} catch(AMQPExchangeException $ex) {
	print_r($ex);
} catch(AMQPChannelException $ex) {
	print_r($ex);
} catch(AMQPConnectionException $ex) {
	print_r($ex);
} catch(Exception $ex) {
	print_r($ex);
}

$connection->disconnect();
