#!/usr/bin/env php
<?php
/**
 rpc_server.php 
 @author: Chimdi Azubuike <me@chimdi.com>
 */

function fib($n) {
	if($n == 0)
		return 0;
	if($n == 1)
		return 1;
	return fib($n - 1) + fib($n - 2);
}

function fast_fib($n) {
	if ($n < 0)
		throw new Exception('Negative number not implemented');
	else
		return fast_fib_calc($n)[0];	
}


function fast_fib_calc($n) {
	if ($n == 0)
		return array(0, 1);
	else {
		list($a,$b) = fast_fib_calc(floor($n/2));
		$c = $a * ($b * 2 - $a);
		$d = $a * $a + $b * $b;
		if (($n % 2) == 0)
			return array($c, $d);
		else
			return array($d, $c + $d);
	}
}

//Establish connection to AMQP
$connection = new AMQPConnection();
$connection->setHost('127.0.0.1');
$connection->setLogin('guest');
$connection->setPassword('guest');
$connection->connect();


//Declare Channel
$channel = new AMQPChannel($connection);
$channel->setPrefetchCount(1);

$exchange = new AMQPExchange($channel);

$queueName = 'rpc_queue';
$queue = new AMQPQueue($channel);
$queue->setName($queueName);
$queue->declareQueue();



echo " [x] Awaiting RPC requests", PHP_EOL;
$callback_func = function(AMQPEnvelope $message, AMQPQueue $q) use (&$exchange) {
	$n = intval($message->getBody());
	echo " [.] fib({$n})", PHP_EOL;

	$attributes = array(
		'correlation_id' => $message->getCorrelationId()
	);

	echo sprintf(" QueueName: %s", $q->getName()), PHP_EOL;
	echo sprintf(" ReplyTo: %s", $message->getReplyTo()), PHP_EOL;
	echo sprintf(" CorrelationID: %s", $message->getCorrelationId()), PHP_EOL;

	$exchange->publish(	(string)fast_fib($n),
						$message->getReplyTo(), 
						AMQP_NOPARAM,
						$attributes
	);
	
	$q->nack($message->getDeliveryTag());
};


try {
	$queue->consume($callback_func);
} catch(AMQPQueueException $ex) {
	print_r($ex);
} catch(Exception $ex) {
	print_r($ex);
}
