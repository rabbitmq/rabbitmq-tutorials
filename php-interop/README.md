# PHP code for RabbitMQ tutorials based on AMQP interop

Here you can find PHP code examples from [RabbitMQ
tutorials](http://www.rabbitmq.com/getstarted.html) adopted to [amqp interop](https://github.com/queue-interop/queue-interop#amqp-interop)
These examples will work with any amqp interop compatible transports such as [enqueue/amqp-ext](https://github.com/php-enqueue/enqueue-dev/blob/master/docs/transport/amqp.md), [enqueue/amqp-bunny](https://github.com/php-enqueue/enqueue-dev/blob/master/docs/transport/amqp_bunny.md), [enqueue/amqp-lib](https://github.com/php-enqueue/enqueue-dev/blob/master/docs/transport/amqp_lib.md)

To successfully use the examples you will need a running RabbitMQ server.

## Requirements

### PHP 5.5+

You need `PHP 5.5` and one of the amqp interop compatible transport.


### Composer

Then [install Composer](https://getcomposer.org/download/) per instructions on their site.

### Client Library

Then you can, for example, install `enqueue/amqp-bunny` using [Composer](http://getcomposer.org).

To do that install Composer and add it to your path, then run the following command
inside this project folder:

```bash
$ composer require enqueue/amqp-bunny
```

