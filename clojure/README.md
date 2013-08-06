# Clojure code for RabbitMQ tutorials

Here you can find Ruby code examples from
[RabbitMQ tutorials](http://www.rabbitmq.com/getstarted.html).

## Requirements

To run this code you need [Langohr](http://clojurerabbitmq.info).

Dependencies are managed by [Leiningen](http://leiningen.org).

These tutorials only require JDK 6 or 7 (Oracle or OpenJDK).

## Code

Code examples are executed via `lein run`:

[Tutorial one: "Hello World!"](http://www.rabbitmq.com/tutorial-one-java.html):

    lein run -m rabbitmq.tutorials.send
    lein run -m rabbitmq.tutorials.receive

[Tutorial two: Work Queues](http://www.rabbitmq.com/tutorial-two-java.html):

    lein run -m rabbitmq.tutorials.new_task
    lein run -m rabbitmq.tutorials.worker

[Tutorial three: Publish/Subscribe](http://www.rabbitmq.com/tutorial-three-java.html)

    lein run -m rabbitmq.tutorials.receive_logs
    lein run -m rabbitmq.tutorials.emit_log

[Tutorial four: Routing](http://www.rabbitmq.com/tutorial-four-java.html)

    lein run -m rabbitmq.tutorials.receive_logs_direct
    lein run -m rabbitmq.tutorials.emit_log_direct

[Tutorial five: Topics](http://www.rabbitmq.com/tutorial-five-java.html)

    lein run -m rabbitmq.tutorials.receive_logs_topic
    lein run -m rabbitmq.tutorials.emit_log_topic

[Tutorial six: RPC](http://www.rabbitmq.com/tutorial-six-java.html)

    lein run -m rabbitmq.tutorials.rpc_server
    lein run -m rabbitmq.tutorials.rpc_client

To learn more, visit [Langohr documentation](http://clojurerabbitmq.info) site.
