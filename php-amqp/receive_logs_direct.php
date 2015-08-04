<?php

//Establish Connection
$connection = new AMQPConnection();
$connection->setHost('127.0.0.1');
$connection->setLogin('guest');
$connection->setPassword('guest');
$connection->connect();


//Listen on Channel
$channel = new AMQPChannel($connection);


echo " [*] Waiting for logs. To exit press CTRL+C", PHP_EOL;
$callback_func = function(AMQPEnvelope $message, AMQPQueue $q) {
	echo sprintf(" [X] [%s] %s",$message->getRoutingKey(),$message->getBody()), PHP_EOL;
	$q->nack($message->getDeliveryTag());
	return true;
};
	
$severities = array_slice($argv,1);
if(empty($severities)) {
	file_put_contents('php://stderr', "Usage: {$argv[0]} [info] [warning] [error]\n");
	exit(1);
}

try {
	//Declare Exchange
	$exchange_name = 'direct_logs';
	$exchange = new AMQPExchange($channel);
	$exchange->setType(AMQP_EX_TYPE_DIRECT);
	$exchange->setName($exchange_name);
	$exchange->declareExchange();



	//Declare Queue
	$queue = new AMQPQueue($channel);
	$queue->setFlags(AMQP_EXCLUSIVE);
	$queue->declareQueue();
	foreach($severities as $routing_key) {
		$queue->bind($exchange_name, $routing_key);
	}

	$queue->consume($callback_func);
} catch(AMQPQueueException $ex) {
	print_r($ex);
} catch(Exception $ex) {
	print_r($ex);
}
