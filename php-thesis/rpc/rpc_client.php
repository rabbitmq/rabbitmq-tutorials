<?php

declare(strict_types=1);

require_once __DIR__.'/../vendor/autoload.php';

use Thesis\Amqp\Client;
use Thesis\Amqp\Config;
use Thesis\Amqp\Message;
use Thesis\Amqp\Rpc;

$client = new Client(
    new Config(
        urls: ['rabbitmq:5672'],
    ),
);

$rpc = new Rpc($client);
$response = $rpc->request(
    new Message('30'),
    routingKey: 'rpc_queue',
);

echo " [.] Got {$response->body} \n";
