<?php

class FibonacciRpcClient {
	protected $connection;
	protected $exchange;
	protected $callbackQueue;
	protected $callbackQueueName;
	protected $response;
	protected $corrId;

	public function __construct() {
		$this->connection = new AMQPConnection();
        $this->connection->connect();

        $this->exchange = new AMQPExchange($this->connection, '');

        $queue = new AMQPQueue($this->connection);
        $this->callbackQueueName = 'rpc_' . uniqid();
        $queue->declare($this->callbackQueueName);
        $this->callbackQueue = $queue;

	}

	public function onResponse($rep) {
        $this->response = $rep['message_body'];
	}

	public function call($n) {
		$this->response = null;
		$this->corrId = uniqid();

        $this->exchange->publish(
			(string) $n,
            'rpc_queue',
            0,
            array(
                'message_id' => $this->corrId,
                'reply_to' => $this->callbackQueueName
            )
        );

        $options = array(
            'min' => 1,
            'max' => 1,
            'ack' => false
        );

        while (!$this->response) {
            while($messages = $this->callbackQueue->consume()) {
                foreach($messages as $message) {
                    $this->onResponse($message);
                }
                if ($this->response) {
                    return $this->response;
                }
            }
        }
	}
};

$start = 30;
if (!empty($argv[1])) {
    $start = $argv[1];
}
$fibonacciRpc = new FibonacciRpcClient();
$response = $fibonacciRpc->call($start);

echo " [.] Got $response\n";
