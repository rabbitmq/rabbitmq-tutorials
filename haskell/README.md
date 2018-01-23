# Haskell code for RabbitMQ tutorials

Here you can find Haskell code examples from
[RabbitMQ tutorials](http://www.rabbitmq.com/getstarted.html).

## Requirements

To run this code you need [Network.AMQP](http://hackage.haskell.org/package/amqp).

### Running the examples with `stack`

1. Install [`stack`](https://docs.haskellstack.org/en/stable/README/).
2. Run the scripts via ```stack FILE ARGS``` instead of `runhaskell FILE ARGS`. (This installs `ghc`, plus `amqp` and other required packages for you.)

## Code

Code examples are executed via `runhaskell`:

[Tutorial one: "Hello World!"](http://www.rabbitmq.com/tutorial-one-python.html):

    runhaskell send.hs
    runhaskell receive.hs

[Tutorial two: Work Queues](http://www.rabbitmq.com/tutorial-two-python.html):

    runhaskell newTask.hs hello world
    runhaskell worker.hs

[Tutorial three: Publish/Subscribe](http://www.rabbitmq.com/tutorial-three-python.html)

    runhaskell receiveLogs.hs
    runhaskell emitLog.hs hello world

[Tutorial four: Routing](http://www.rabbitmq.com/tutorial-four-python.html)

    runhaskell receiveLogsDirect.hs info warn
    runhaskell emitLogDirect.hs warn "a warning"

[Tutorial five: Topics](http://www.rabbitmq.com/tutorial-five-python.html)

    runhaskell receiveLogsTopic.hs info warn
    runhaskell emitLogTopic.hs warn "a warning"

[Tutorial six: RPC](http://www.rabbitmq.com/tutorial-six-python.html)

    runhaskell rpcServer.hs
    runhaskell rpcClient.hs

To learn more, see [Network.AMQP](https://github.com/hreinhardt/amqp).
