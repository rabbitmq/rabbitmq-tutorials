<?php

//Establish connection to AMQP
$connection = new AMQPConnection();
$connection->setHost('127.0.0.1');
$connection->setLogin('guest');
$connection->setPassword('guest');
$connection->connect();


//Declare Channel
$channel = new AMQPChannel($connection);
$channel->setPrefetchCount(1);

//Declare Exchange
$exchange_name = 'topic_logs';
$exchange = new AMQPExchange($channel);
$exchange->setType(AMQP_EX_TYPE_TOPIC);
$exchange->setName($exchange_name);
$exchange->declareExchange();

$binding_keys = array_slice($argv, 1); //accept an array of inputs delimited by space
if(empty($binding_keys)) {
	file_put_contents('php://stderr', "Usage: {$argv[0]} [binding_key]...\n");
	exit(1);		
}

//Declare Queue
$queue = new AMQPQueue($channel);
$queue->setFlags(AMQP_EXCLUSIVE);
$queue->declareQueue();
foreach($binding_keys as $binding_key) {
	$queue->bind($exchange_name, $binding_key);
}

echo " [*] Waiting for logs. To exit press CTRL+C", PHP_EOL;
$callback_func = function(AMQPEnvelope $message, AMQPQueue $q) {
	echo sprintf(" [X] [%s] %s",$message->getRoutingKey(),$message->getBody()), PHP_EOL;
	//echo sprintf("Delivery Tag: %s",$message->getDeliveryTag()), PHP_EOL;
	$q->nack($message->getDeliveryTag());
};


try {
	$queue->consume($callback_func);
} catch(AMQPQueueException $ex) {
	print_r($ex);
} catch(Exception $ex) {
	print_r($ex);
}
