# Rust code for RabbitMQ tutorials

Here you can find the Rust code examples for [RabbitMQ
tutorials](https://www.rabbitmq.com/getstarted.html).

The examples use [lapin](https://github.com/CleverCloud/lapin) client library.

You should have a RabbitMQ server running on default port.

## Requirements

* [Rust and Cargo](https://www.rust-lang.org/tools/install)

## Code
Each cargo command should be launched in a separate shell.

#### [Tutorial one: "Hello World!"](https://www.rabbitmq.com/tutorial-one-python.html)

    cargo run --bin receive
    cargo run --bin send

#### [Tutorial two: Work Queues](https://www.rabbitmq.com/tutorial-two-python.html)

    cargo run --bin worker
    cargo run --bin new_task "hi" # specify a custom message

#### [Tutorial three: Publish/Subscribe](https://www.rabbitmq.com/tutorial-three-python.html)

    cargo run --bin receive_logs
    cargo run --bin emit_log "hi" # specify a custom message

#### [Tutorial four: Routing](https://www.rabbitmq.com/tutorial-four-python.html)

    cargo run --bin receive_logs_direct info error # specify log levels
    cargo run --bin emit_log_direct error "help!" # specify severity and custom message

#### [Tutorial five: Topics](https://www.rabbitmq.com/tutorial-five-python.html)

    cargo run --bin receive_logs_topic kern.* # specify topic filter
    cargo run --bin emit_log_topic kern.mem "No memory left!" # specify topic and message

#### [Tutorial six: RPC](https://www.rabbitmq.com/tutorial-six-python.html)

    cargo run --bin rpc_server
    cargo run --bin rpc_client
