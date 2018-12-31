# C code for RabbitMQ tutorials #

This code is using [RabbitMQ C AMQP client libarary](https://github.com/alanxz/rabbitmq-c).
And is based on the examples taken from there.

## Requirements

To run this code you need a C compiler (e.g. gcc) and ```make```.
You would also need to install the rabbitmq-c development package. On Redhat/CentOS/Fedora machines, this would be:
```
# sudo yum install librabbotmq-devel
```

## Code

[C implementation of Tutorial five: Topics](http://www.rabbitmq.com/tutorial-five-python.html):
To compile the examples use:
```
# make
```
## [Example 1](https://www.rabbitmq.com/tutorials/tutorial-one-python.html)
Sending and receiving a "Hello World!" message over a queue called "hello".
In one terminal:
```
./receive localhost 5672 hello
```
And from another terminal:
```
./send localhost 5672 hello
```
## [Example 5](https://www.rabbitmq.com/tutorials/tutorial-five-python.html)
Sending and receiving a "Hello World!" message using an exchange called "topic_logs". Receiver will get all messages that use a topic that ends with "critical".
In one terminal:
```
./receive_logs_topic localhost 5672 topic_logs "*.critical"
```
And from another terminal:
```
./emit_log_topic localhost 5672 topic_logs "kern.critical"
```

