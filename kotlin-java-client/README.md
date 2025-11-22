# RabbitMQ Tutorials in Kotlin

This is a minimalistic Kotlin port of the [RabbitMQ tutorials in Java](https://www.rabbitmq.com/getstarted.html).
The port is admittedly quite close to Java in terms of code style.


## Compiling the Code

``` shell
gradle clean compileKotlin
```

## Running the Tutorials

### Tutorial 1

Execute the following command to start a Hello, world consumer

``` shell
gradle run -P main=Recv
```

Execute the following in a separate shell to publish a Hello, world messge:

``` shell
gradle run -P main=Send
```

### Tutorial 2

Send a task message. The task will be completed immediately

``` shell
gradle run -P main=NewTask
```

To start a worker (run in a separate shell):

``` shell
gradle run -P main=Worker
```

Send a task message. It will wait for 1 second for each dot in the payload.

``` shell
gradle run -P main=NewTask -P argv="rabbit1 ...."
```

Add more workers to the same queue, message will be distributed in the
round robin manner.

### Tutorial 3

``` shell
gradle run -P main=ReceiveLogs
```


``` shell
gradle run -P main=EmitLog -P argv="rabbit1, msg1"
```

### Tutorial 4

``` shell
gradle run -P main="ReceiveLogsDirect" -P argv="info,error"
```

``` shell
gradle run -P main=EmitLogDirect"
```

### Tutorial 5

``` shell
gradle run -P main=ReceiveLogsTopic -P argv="anonymous.*"
```

``` shell
gradle run -P main=EmitLogTopic -P argv="anonymous.info"
```

### Tutorial 6

In one shell:

``` shell
gradle run -P main=RPCServer
```

In another shell:

``` shell
gradle run -P main=RPCClient
```
