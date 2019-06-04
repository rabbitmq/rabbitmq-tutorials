# Rust code for RabbitMQ tutorials

Here you can find the Rust code examples for [RabbitMQ
tutorials](https://www.rabbitmq.com/getstarted.html).

The examples use [lapin](https://github.com/sozu-proxy/lapin) client library.

To successfully use the examples you will need a running RabbitMQ server.

## Requirements

* [Rust and Cargo](https://www.rust-lang.org/tools/install)

## Code
Each tutorial is a separate crate where each source file corresponds to a
binary executable.

#### [Tutorial one: "Hello World!"](https://www.rabbitmq.com/tutorial-one-dotnet.html)
```
cd 01-hello-world
```
Start receiver and sender in two separate shells:
```
cargo run --bin receive
cargo run --bin send
```

#### [Tutorial two: Work Queues](https://www.rabbitmq.com/tutorial-two-dotnet.html)
// TODO

#### [Tutorial three: Publish/Subscribe](https://www.rabbitmq.com/tutorial-three-dotnet.html)
// TODO

#### [Tutorial four: Routing](https://www.rabbitmq.com/tutorial-four-dotnet.html)
// TODO

#### [Tutorial five: Topics](https://www.rabbitmq.com/tutorial-five-dotnet.html)
// TODO

#### [Tutorial six: RPC](https://www.rabbitmq.com/tutorial-six-dotnet.html)

