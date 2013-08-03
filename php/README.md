# PHP code for RabbitMQ tutorials

Here you can find PHP code examples from [RabbitMQ
tutorials](http://www.rabbitmq.com/getstarted.html).

To successfully use the examples you will need a running RabbitMQ server.

## Requirements

Additionally you need `PHP 5.3` and `php-amqplib`. To get these
dependencies on Ubuntu type:

    sudo apt-get install git-core php5-cli

Then you can install `php-amqplib` using [Composer](http://getcomposer.org).

To do that install Composer and add it to your path, then run the following command
inside this project folder:

    composer.phar install

## Code

[Tutorial one: "Hello World!"](http://www.rabbitmq.com/tutorial-one-python.html):

    php send.php
    php receive.php


[Tutorial two: Work Queues](http://www.rabbitmq.com/tutorial-two-python.html):

    php new_task.php "A very hard task which takes two seconds.."
    php worker.php


[Tutorial three: Publish/Subscribe](http://www.rabbitmq.com/tutorial-three-python.html)

    php receive_logs.php
    php emit_log.php "info: This is the log message"

[Tutorial four: Routing](http://www.rabbitmq.com/tutorial-four-python.html):

    php receive_logs_direct.php info
    php emit_log_direct.php info "The message"


[Tutorial five: Topics](http://www.rabbitmq.com/tutorial-five-python.html):

    php receive_logs_topic.php "*.rabbit"
    php emit_log_topic.php red.rabbit Hello

[Tutorial six: RPC](http://www.rabbitmq.com/tutorial-six-python.html):

    php rpc_server.php
    php rpc_client.php
