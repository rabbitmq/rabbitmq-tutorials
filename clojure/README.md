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

    lein run -m "rabbitmq.tutorials.send"
    lein run -m "rabbitmq.tutorials.receive"

[Tutorial two: Work Queues](http://www.rabbitmq.com/tutorial-two-java.html):

    lein run -m "rabbitmq.tutorials.new-task"
    lein run -m "rabbitmq.tutorials.worker"

[Tutorial three: Publish/Subscribe](http://www.rabbitmq.com/tutorial-three-java.html)

    ruby receive_logs.rb
    ruby emit_log.rb

[Tutorial four: Routing](http://www.rabbitmq.com/tutorial-four-java.html)

    ruby receive_logs_direct.rb
    ruby emit_log_direct.rb

[Tutorial five: Topics](http://www.rabbitmq.com/tutorial-five-java.html)

    ruby receive_logs_topic.rb
    ruby emit_log_topic.rb

[Tutorial six: RPC](http://www.rabbitmq.com/tutorial-six-java.html)

    ruby rpc_server.rb
    ruby rpc_client.rb

To learn more, visit [Langohr documentation](http://clojurerabbitmq.info) site.
