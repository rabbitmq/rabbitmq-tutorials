# Haskell code for RabbitMQ tutorials

Here you can find Clojure code examples from
[RabbitMQ tutorials](http://www.rabbitmq.com/getstarted.html).

## Requirements

To run this code you need [Network.AMQP](http://hackage.haskell.org/package/amqp-0.6.0/docs/Network-AMQP.html).

## Code

Code examples are executed via `runhaskell`:

[Tutorial one: "Hello World!"](http://www.rabbitmq.com/tutorial-one-python.html):

    runhaskell send.hs
    runhaskell receive.hs

[Tutorial two: Work Queues](http://www.rabbitmq.com/tutorial-two-python.html):

    runhaskell new_task.hs
    runhaskell worker.hs

[Tutorial three: Publish/Subscribe](http://www.rabbitmq.com/tutorial-three-python.html)

    runhaskell receive_logs.hs
    runhaskell emit_log.hs

[Tutorial four: Routing](http://www.rabbitmq.com/tutorial-four-python.html)

    runhaskell receive_logs_direct.hs
    runhaskell emit_log_direct.hs

[Tutorial five: Topics](http://www.rabbitmq.com/tutorial-five-python.html)

    runhaskell receive_logs_topic.hs
    runhaskell emit_log_topic.hs

[Tutorial six: RPC](http://www.rabbitmq.com/tutorial-six-python.html)

    TBD

To learn more, visit [Langohr documentation](http://clojurerabbitmq.info) site.
