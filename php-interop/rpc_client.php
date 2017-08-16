<?php

// composer require enqueue/amqp-bunny
require_once __DIR__.'/vendor/autoload.php';

use Enqueue\AmqpBunny\AmqpConnectionFactory;

$config = [
    'host' => 'localhost',
    'port' => 5672,
    'user' => 'guest',
    'pass' => 'guest',
    'receive_method' => 'basic_consume',
];

class FibonacciRpcClient
{
    /** @var \Interop\Amqp\AmqpContext */
    private $context;

    /** @var \Interop\Amqp\AmqpQueue */
    private $callback_queue;

    public function __construct(array $config)
    {
        $this->context = (new AmqpConnectionFactory($config))->createContext();
        $this->callback_queue = $this->context->createTemporaryQueue();
    }

    public function call($n)
    {
        $corr_id = uniqid();

        $message = $this->context->createMessage((string) $n);
        $message->setCorrelationId($corr_id);
        $message->setReplyTo($this->callback_queue->getQueueName());

        $this->context->createProducer()->send(
            $this->context->createQueue('rpc_queue'),
            $message
        );

        $consumer = $this->context->createConsumer($this->callback_queue);

        while (true) {
            if ($message = $consumer->receive()) {
                if ($message->getCorrelationId() == $corr_id) {
                    return (int) ($message->getBody());
                }
            }
        }
    }
}

$fibonacci_rpc = new FibonacciRpcClient($config);
$response = $fibonacci_rpc->call(30);
echo ' [.] Got ', $response, "\n";
