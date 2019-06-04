# Rust code for RabbitMQ tutorials

Here you can find the Rust code examples for [RabbitMQ
tutorials](https://www.rabbitmq.com/getstarted.html).

The examples use [lapin](https://github.com/sozu-proxy/lapin) client library.

To successfully use the examples you will need a running RabbitMQ server.

## Requirements

* [Rust and Cargo](https://www.rust-lang.org/tools/install)

## Code
Each tutorial is a separate crate where each source file corresponds to a
binary executable. Each cargo command should be launched in a separate shell.

#### [Tutorial one: "Hello World!"](https://www.rabbitmq.com/tutorial-one-dotnet.html)
```
cd 01-hello-world
```
```
cargo run --bin receive
cargo run --bin send
```

#### [Tutorial two: Work Queues](https://www.rabbitmq.com/tutorial-two-dotnet.html)
```
cd 02-work-queues
```
```
cargo run --bin worker
cargo run --bin new-task "hi" # specify a custom message
```

#### [Tutorial three: Publish/Subscribe](https://www.rabbitmq.com/tutorial-three-dotnet.html)
```
cd 03-publish-subscribe
```
```
cargo run --bin receive
cargo run --bin emit "hi" # specify a custom message
```

#### [Tutorial four: Routing](https://www.rabbitmq.com/tutorial-four-dotnet.html)
```
cd 04-routing
```
```
cargo run --bin receive info error # specify log levels
cargo run --bin emit-direct error "help!" # specify severity and custom message
```

#### [Tutorial five: Topics](https://www.rabbitmq.com/tutorial-five-dotnet.html)
```
cd 05-topics
```
```
cargo run --bin receive-topic kern.* # specify topic filter
cargo run --bin emit-topic kern.mem "No memory left!" # specify topic and message
```

