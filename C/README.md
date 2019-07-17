# C code for RabbitMQ tutorials #

This code is using [RabbitMQ C AMQP client library](https://github.com/alanxz/rabbitmq-c).
And is based on the examples taken from there.

## Requirements

To run this code you need a C compiler (e.g. `gcc`) and `make`.
You would also need to install the rabbitmq-c development package.

On Redhat/CentOS/Fedora machines, this would be:

```
sudo yum install librabbotmq-devel
```

On OSX machines, this would be: 

```
brew install rabbitmq-c
```

## Code

To compile the examples use:

```
make
```

#### [Tutorial one: "Hello World!"](https://www.rabbitmq.com/tutorial-one-python.html):

```
./receive localhost 5672 hello
./send localhost 5672 hello
```

#### [Tutorial two: Work Queues](https://www.rabbitmq.com/tutorial-two-python.html):

```
./new_task "A very hard task which takes two seconds.."
./worker
```
    
#### [Tutorial three: Publish/Subscribe](https://www.rabbitmq.com/tutorial-three-go.html)

```
./receive_logs
./emit_log "info: This is the log message"
```

#### [Tutorial four: Routing](https://www.rabbitmq.com/tutorial-four-go.html)

```
./receive_logs_direct info
./emit_log_direct info "The message"
```

#### [Tutorial five: Topics](https://www.rabbitmq.com/tutorial-five-go.html)

```
./receive_logs_topic "*.critical"
./emit_log_topic "kern.critical"
```

#### [Tutorial six: RPC](https://www.rabbitmq.com/tutorial-six-go.html)

```
./rpc_server
./rpc_client
```