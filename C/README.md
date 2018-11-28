# C code for RabbitMQ tutorials #

This code is using [RabbitMQ C AMQP client libarary](https://github.com/alanxz/rabbitmq-c).
And is based on the examples taken from there.

## Requirements

To run this code you need a C compiler (e.g. gcc). 
You would also need to install the rabbitmq-c development package. On Redhat/CentOS/Fedora machines, this would be:
```
# sudo yum install librabbotmq-devel
```

## Code

[C implementation of Tutorial five: Topics](http://www.rabbitmq.com/tutorial-five-python.html):
To compile (using gcc) use:
```
# gcc -Wall -o emit_log_topic emit_log_topic.c utils.c -lrabbitmq
# gcc -Wall -o receive_logs_topic receive_logs_topic.c utils.c -lrabbitmq
```

Then run in one terminal:
```
./receive_logs_topic localhost 5672 topic_logs topic1
```
And from another terminal:
```
./emit_log_topic localhost 5672 topic_logs topic1
```

