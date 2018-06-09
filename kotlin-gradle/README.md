# RabbitMQ Tutorials in Kotlin

This is a minimalistic Kotlin port of the RabbitMQ tutorials in Java.
The port is admittedly quite close to Java in terms of code style.
This is primarily to the fact that RabbitMQ Java client still supports
JDK 8 and doesn't have a lambda-friendly API.


## Compiling the Code

```console
$ gradle clean compileKotlin
```

## Running Examples

### Hello World

Execute the following command to receive a hello world:

```console
$ gradle run -P main=Recv
```

Execute the following in a separate shell to send a hello world:

```console
$ gradle run -P main=Send
```

### Work Queues

Send a message which will be finshed immediately:

```console
$ gradle run -P main=NewTask
```

Send a message which need some second to execute each . is one second.

```console
$ gradle run -P main=NewTask -P argv="rabbit1 ...."
```

To start a worker (run in a separate shell):

```console
$ gradle run -P main=Worker
```

Add more workers to the same queue, message will be distributed in the
round robin manner.

### Publish and Subscriber

```console
$ gradle run -P main=EmitLog -P argv="rabbit1, msg1"
```
```console
$ gradle run -P main=ReceiveLogs
```

### RPC

In one shell:

```console
$ gradle run -P main=RPCServer
```

In another shell:

```console
$ gradle run -P main=RPCClient
```
