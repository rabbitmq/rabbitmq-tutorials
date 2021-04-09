# Common Lisp code for RabbitMQ tutorials

Here you can find Common Lisp code examples from
[RabbitMQ tutorials](https://cl-rabbit.io/cl-bunny/tutorials/).

## Requirements

To run this code you need [Cl-Bunny](https://cl-rabbit.io/cl-bunny).

You can install it via Quicklisp:

    (ql:quickload :cl-bunny)

Note: Cl-Bunny developed and tested using x64 sbcl on GNU/Linux

All our examples are in fact executable sbcl scripts. You can run them from command line

## Code

Tutorial one: "Hello World!"]:

    ./send.lisp
    ./receive.lisp

Tutorial two: Work Queues:

    ./new-task.lisp
    ./worker.lisp

Tutorial three: Publish/Subscribe

    ./receive-logs.lisp
    ./emit-log.lisp

Tutorial four: Routing

    ./receive-logs-direct.lisp
    ./emit-log-direct.lisp

Tutorial five: Topics

    ./receive-logs-topic.lisp
    ./emit-log-topic.lisp

Tutorial six: RPC

    ./rpc-server.lisp
    ./rpc-client.lisp

To learn more, visit [Cl-Bunny documentation](https://cl-rabbit.io/cl-bunny) site.
