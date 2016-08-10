<?php

$connection = new AMQPConnection();
$connection->connect();

$exchange = new AMQPExchange($connection, '');

$queue = new AMQPQueue($connection);
$queue->declare('rpc_queue');

function fib($n) {
	if (!$n) {
		return 0;
    }
	if ($n == 1) {
		return 1;
    }
	return fib($n-1) + fib($n-2);
}

echo " [x] Awaiting RPC requests\n";

$callback = function($req) use ($queue, $exchange) {
    $n = intval($req['message_body']);
    echo " [.] fib($n)\n";

    $msg = fib($n);

    $exchange->publish(
        $msg,
        $req['Reply-to']
    );
    $queue->ack($req['delivery_tag']);

};

$options = array(
    'min' => 1,
    'max' => 1,
    'ack' => false
);
while ($messages = $queue->consume($options)) {
    foreach($messages as $message) {
        $callback($message);
    }
}
