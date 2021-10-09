# RabbitMQ Tutorials

This project contains code for [RabbitMQ tutorials](https://www.rabbitmq.com/getstarted.html) with
their ports to various languages.

This repository only contains runnable code. Please consult [tutorials on the site](https://www.rabbitmq.com/getstarted.html)
to learn more about [the concepts](https://www.rabbitmq.com/getstarted.html), requirements, supported client library version and so on.

And please check out the rest of the [RabbitMQ documentation](https://www.rabbitmq.com/documentation.html)!

## Prerequisites

All tutorials **require a RabbitMQ node running on localhost** with stock (default) settings.

Please refer to RabbitMQ documentation to learn
more about various [installation options](https://www.rabbitmq.com/download.html):

 * A [Windows installer](https://www.rabbitmq.com/install-windows.html)
 * A [Docker image](https://hub.docker.com/_/rabbitmq/)
 * [Homebrew](https://www.rabbitmq.com/install-homebrew.html) on MacOS
 * Packages for [Ubuntu and Debian](https://www.rabbitmq.com/install-debian.html) as well as [RPM-based distributions](https://www.rabbitmq.com/install-rpm.html) 
 * A generic [binary build](https://www.rabbitmq.com/install-generic-unix.html) for Linux, *BSD and other UNIX-like systems

## Languages

The following ports are available:

 * [C#](./dotnet)
 * [C# (with Visual Studio)](./dotnet-visual-studio)
 * [C# (.NET 6.0)](./dotnet-6)
 * [Clojure](./clojure)
 * [Common Lisp](./common-lisp)
 * [Dart](./dart)
 * [Elixir](./elixir) 
 * [Erlang](./erlang)
 * [Go](./go)
 * [Haskell](./haskell)
 * [JavaScript (with Node and amqp-node)](./javascript-nodejs) (using callbacks)
 * [JavaScript (with Node and amqp-node)](https://github.com/squaremo/amqp.node/tree/master/examples) (using promises/futures)
 * [Java with Maven](./java-mvn)
 * [Java (with IntelliJ IDEA)](./java-idea)
 * [Java](./java) (with manual dependency management)
 * [Kotlin](./kotlin)
 * [PHP (with php-amqplib)](./php)
 * [PHP (with php-amqp)](./php-amqp)
 * [PHP (with queue-interop)](./php-interop)
 * [Perl](./perl)
 * [Python (with Pika)](./python)
 * [Ruby (with Bunny)](./ruby)
 * [Rust](./rust)
 * [Scala](./scala)
 * [Swift](./swift)
 * [Spring AMQP](./spring-amqp)
 * [SoapUI](./soapui)
 
## License

Released under the [Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0.txt).
