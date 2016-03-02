# Common Lisp code for RabbitMQ tutorials

Here you can find Common Lisp code examples from
[RabbitMQ tutorials](http://cl-rabbit.io/cl-bunny/tutorials/).

## Requirements

To run this code you need [Cl-Bunny](http://cl-rabbit.io/cl-bunny).

You can install it via Quicklisp:

    (ql:quickload :cl-bunny)

Note: Cl-Bunny developed and tested using x64 sbcl on GNU/Linux

All our examples are in fact executable sbcl scripts. You can run them from command line

## Code

[Tutorial one: "Hello World!"](http://cl-rabbit.io/cl-bunny/tutorials/tutorial-one-cl.html):

    ./send.lisp
    ./receive.lisp

[Tutorial two: Work Queues](http://cl-rabbit.io/cl-bunny/tutorials/tutorial-two-cl.html):

    ./new-task.lisp
    ./worker.lisp

[Tutorial three: Publish/Subscribe](http://cl-rabbit.io/cl-bunny/tutorials/tutorial-three-cl.html)

    ./receive-logs.lisp
    ./emit-log.lisp

[Tutorial four: Routing](http://cl-rabbit.io/cl-bunny/tutorials/tutorial-four-cl.html)

    ./receive-logs-direct.lisp
    ./emit-log-direct.lisp

[Tutorial five: Topics](http://cl-rabbit.io/cl-bunny/tutorials/tutorial-five-cl.html)

    ./receive-logs-topic.lisp
    ./emit-log-topic.lisp

[Tutorial six: RPC](http://cl-rabbit.io/cl-bunny/tutorials/tutorial-six-cl.html)

    ./rpc-server.lisp
    ./rpc-client.lisp

To learn more, visit [Cl-Bunny documentation](http://cl-rabbit.io/cl-bunny) site.
