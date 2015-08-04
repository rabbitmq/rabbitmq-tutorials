<?php


class FibonacciRpcClient {
	private $connection;
	private $channel;

	private $callbackQueueName;
	private $queueName = 'rpc_queue';
	private $rpcQueue = 'rpc_queue';


	private $response;

	protected $queue;
	protected $corrId;

	public function __construct() {	
		$this->connection = $this->getAMQPConnection();		
		$this->setChannel();
		$this->setExchange();
	}

	/**
	 AMQP Connection
	 */
	protected function getAMQPConnection() {
		$connection = new AMQPConnection();
		$connection->setHost('127.0.0.1');
		$connection->setLogin('guest');
		$connection->setPassword('guest');
		$connection->connect();
		return $connection;
	}

	/**
	 Declare Channel
	 */
	protected function setChannel() {
		$this->channel = new AMQPChannel($this->connection);
		$this->channel->setPrefetchCount(1);	
	}

	/**
	 Declare Exchange
	 */
	protected function setExchange() {	
		$this->exchange = new AMQPExchange($this->channel);
	}

	public function on_response(AMQPEnvelope $message, AMQPQueue $queue) {
		print_r(func_get_args());	
	}

	public function call($value) {
		$this->response = NULL;
		$this->corrId = uniqid();
		
		try {
			//Declare an nonymous channel
			$this->queue = new AMQPQueue($this->channel);
			$this->queue->setFlags(AMQP_EXCLUSIVE);
			$this->queue->declareQueue();	
			$this->callbackQueueName = $this->queue->getName();

			//Set Publish Attributes
			$attributes = array(
				'correlation_id' => $this->corrId,
				'reply_to' 		 =>	$this->callbackQueueName
			);

			$this->exchange->publish(
				$value,
				$this->rpcQueue,
				AMQP_NOPARAM,
				$attributes
			);

			$callback = function(AMQPEnvelope $message, AMQPQueue $q) {
				if($message->getCorrelationId() == $this->corrId) {
					//echo sprintf("CorrelationID: %s",$message->getCorrelationId()), PHP_EOL; 
					//echo sprintf("Response: %s",$message->getBody()), PHP_EOL; 
					$this->response = $message->getBody();
					$q->nack($message->getDeliveryTag());
					return false;
				}	
			};	
	
			$this->queue->consume($callback);
			
			//Return RPC Results
			return $this->response;
		} catch(AMQPQueueException $ex) {
			print_r($ex);
		} catch(Exception $ex) {
			print_r($ex);
		}
	}
}

$value = (isset($argv[1]))? $argv[1] : 5;
$fibonacciRpc = new FibonacciRpcClient();
echo sprintf(" [x] Requesting fib(%s)",$value), PHP_EOL;
$response = $fibonacciRpc->call($value);
echo sprintf(" [.] Received: %s",$response), PHP_EOL;
