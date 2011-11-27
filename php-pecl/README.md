# PHP code for RabbitMQ tutorial #

Here you can find a PHP code examples from [RabbitMQ
tutorials](http://www.rabbitmq.com/getstarted.html).


## Requirements ##

To run the examples you need a running RabbitMQ server.

Additionally you need the [PHP AMQP PECL package](http://php.net/manual/en/book.amqp.php)

## Code

[Tutorial one: "Hello World!"](http://www.rabbitmq.com/tutorial-one-python.html):

    php send.php
    php receive.php


[Tutorial two: Work Queues](http://www.rabbitmq.com/tutorial-two-python.html):

    php new_task.php "A very hard task which takes two seconds.."
    php worker.php


[Tutorial three: Publish/Subscribe](http://www.rabbitmq.com/tutorial-three-python.html)

    php receive_logs.php
    php emit_log.php This is the log message


[Tutorial four: Routing](http://www.rabbitmq.com/tutorial-four-python.html)

    php receive_logs_direct.php info
    php emit_log_direct.php info The message


[Tutorial five: Topics](http://www.rabbitmq.com/tutorial-five-python.html)

    php receive_logs_topic.php '*.rabbit'
    php emit_log_topic.php red.rabbit Hello


[Tutorial six: RPC](http://www.rabbitmq.com/tutorial-six-python.html):

    php rpc_server.php
    php rpc_client.php
