# Objective-C code for RabbitMQ tutorials

Objective-C code examples for the [RabbitMQ tutorials](http://www.rabbitmq.com/getstarted.html).

## Requirements

To run this code you need
[Carthage](https://github.com/Carthage/Carthage) to pull down dependencies,
which include the
[Objective-C client](https://github.com/rabbitmq/rabbitmq-objc-client) itself.

If you have Homebrew installed, simply:

```sh
brew install carthage
```

You also need a running RabbitMQ server on localhost.

## Installation

Each tutorial has its own Xcode project. Before the projects can be run, you
need to download and build their dependencies.

For example, to install tutorial 1:

```sh
cd tutorial1
carthage bootstrap --platform iOS
```

You should then be able to open [the project](tutorial1/tutorial1.xcodeproj) in Xcode and hit Run. Output is
NSLogged.

See [ViewController.m](tutorial1/tutorial1/ViewController.m) for the
implementation (each tutorial has its own `ViewController.m`).
