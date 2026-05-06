# Zig code for RabbitMQ tutorials

This is a Zig port of the
[RabbitMQ tutorials](https://www.rabbitmq.com/tutorials).

These examples use [`bunny-zig`](https://github.com/michaelklishin/bunny-zig),
a Zig client library for RabbitMQ.


## Requirements

These tutorials require Zig 0.16.0 or later and a local RabbitMQ node listening
on `localhost:5672` with default credentials (`guest`).

To fetch dependencies and build the tutorials:

``` sh
zig build
```

Built binaries can then be found under under `zig-out/bin/`.


## Code

To run [tutorial one: "Hello World!"](https://www.rabbitmq.com/tutorials/tutorial-one-zig.html):

``` sh
zig build run-send
zig build run-receive
```

[Tutorial two: Work Queues](https://www.rabbitmq.com/tutorials/tutorial-two-zig.html):

``` sh
zig build run-new_task -- "First message."
zig build run-worker
```

[Tutorial three: Publish/Subscribe](https://www.rabbitmq.com/tutorials/tutorial-three-zig.html):

``` sh
zig build run-receive_logs
zig build run-emit_log
```

[Tutorial four: Routing](https://www.rabbitmq.com/tutorials/tutorial-four-zig.html):

``` sh
zig build run-receive_logs_direct -- info warning error
zig build run-emit_log_direct -- info "Hello, world!"
```

[Tutorial five: Topics](https://www.rabbitmq.com/tutorials/tutorial-five-zig.html):

``` sh
zig build run-receive_logs_topic -- "*.critical"
zig build run-emit_log_topic -- kern.critical "A critical kernel error"
```

[Tutorial six: RPC](https://www.rabbitmq.com/tutorials/tutorial-six-zig.html):

``` sh
zig build run-rpc_server
zig build run-rpc_client -- 30
```

[Tutorial seven: Publisher Confirms](https://www.rabbitmq.com/tutorials/tutorial-seven-zig.html):

``` sh
zig build run-publisher_confirms
```

The compiled binaries can also be invoked directly, for example:

``` sh
./zig-out/bin/emit_log_direct warning "Disk almost full"
```

To learn more, see the [bunny-zig README](https://github.com/michaelklishin/bunny-zig).
