# PHP code for RabbitMQ tutorial #

Here you can find a PHP code examples from [RabbitMQ
tutorials](http://www.rabbitmq.com/getstarted.html).


## Requirements ##

To run the examples you need a running RabbitMQ server.

Additionally you need `PHP 5.3` and `php-amqplib`. To get these
dependencies on Ubuntu type:

    sudo apt-get install git-core php5-cli
    git clone http://github.com/videlalvaro/php-amqplib.git lib/php-amqplib

Since the tutorials are tested with `php-amqplib` v1.0 you might want to checkout the `v1.0` tag as well:

    cd lib/php-amqplib/
    git checkout v1.0

## Code

[Tutorial one: "Hello World!"](http://www.rabbitmq.com/tutorial-one-python.html):

    php send.php
    php receive.php


[Tutorial two: Work Queues](http://www.rabbitmq.com/tutorial-two-python.html):

    php new_task.php
    php worker.php


[Tutorial three: Publish/Subscribe](http://www.rabbitmq.com/tutorial-three-python.html)

    php receive_logs.php
    php emit_log.php

[Tutorial six: RPC](http://www.rabbitmq.com/tutorial-six-python.html):

    php rpc_server.php
    php rpc_client.php
