# Haskell code for RabbitMQ tutorials

Here you can find Haskell code examples from
[RabbitMQ tutorials](https://www.rabbitmq.com/getstarted.html).

## Requirements

To run this code you need [Network.AMQP](https://hackage.haskell.org/package/amqp).

### Running the examples with `stack`

1. Install [`stack`](https://docs.haskellstack.org/en/stable/README/).
2. Run the scripts via ```stack FILE ARGS``` instead of `runhaskell FILE ARGS`. (This installs `ghc`, plus `amqp` and other required packages for you.)

## Code

Code examples are executed via `runhaskell`.

Tutorial one:

``` shell
runhaskell send.hs
runhaskell receive.hs
```

Tutorial two:

``` shell
runhaskell newTask.hs hello world
runhaskell worker.hs
```

Tutorial three: Publish/Subscribe

``` shell
runhaskell receiveLogs.hs
runhaskell emitLog.hs hello world
```

Tutorial four: Routing

``` shell
runhaskell receiveLogsDirect.hs info warn
runhaskell emitLogDirect.hs warn "a warning"
```

Tutorial five: Topics

``` shell
runhaskell receiveLogsTopic.hs info warn
runhaskell emitLogTopic.hs warn "a warning"
```

Tutorial six: RPC

``` shell
runhaskell rpcServer.hs
runhaskell rpcClient.hs
```

To learn more, see [Network.AMQP](https://github.com/hreinhardt/amqp).
