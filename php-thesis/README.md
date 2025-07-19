# Non-blocking php code for RabbitMQ tutorials based on thesis/amqp

Run rabbitmq server and php container with `make up`.

## Code

[Tutorial one: "Hello World!"](https://www.rabbitmq.com/tutorials/tutorial-one-php):
```shell
make run-send
make run-receive
```

[Tutorial two: Work Queues](https://www.rabbitmq.com/tutorials/tutorial-two-php):
```shell
make run-new-task
make run-worker
```

[Tutorial three: Publish/Subscribe](https://www.rabbitmq.com/tutorials/tutorial-three-php)
```shell
make run-emit-log
make run-receive-logs
```

[Tutorial four: Routing](https://www.rabbitmq.com/tutorials/tutorial-four-php):
```shell
make run-emit-log-direct
make run-receive-logs-direct
```

[Tutorial five: Topics](https://www.rabbitmq.com/tutorials/tutorial-five-php):
```shell
make run-emit-log-topic
make run-receive-logs-topic
```

[Tutorial six: RPC](https://www.rabbitmq.com/tutorials/tutorial-six-php):
```shell
make run-rpc-server
make run-rpc-client
```

[Tutorial seven: Publisher Confirms](https://www.rabbitmq.com/tutorials/tutorial-seven-php):
```shell
make run-rpc-server
make run-rpc-client
```
