# Ruby code for RabbitMQ tutorials

Here you can find Ruby code examples from
[RabbitMQ tutorials](http://www.rabbitmq.com/getstarted.html).

## Requirements

To run this code you need [Bunny 0.9+](http://rubybunny.info).

You can install it via RubyGems:

    gem install bunny --version ">= 0.9.1"

Bunny supports Ruby 2.0, 1.9, JRuby, Rubinius 2.0, and Ruby 1.8.7.

## Code

[Tutorial one: "Hello World!"](http://www.rabbitmq.com/tutorial-one-python.html):

    ruby send.rb
    ruby receive.rb

[Tutorial two: Work Queues](http://www.rabbitmq.com/tutorial-two-python.html):

    ruby new_task.rb
    ruby worker.rb

[Tutorial three: Publish/Subscribe](http://www.rabbitmq.com/tutorial-three-python.html)

    ruby receive_logs.rb
    ruby emit_log.rb

[Tutorial four: Routing](http://www.rabbitmq.com/tutorial-four-python.html)

    ruby receive_logs_direct.rb
    ruby emit_log_direct.rb

[Tutorial five: Topics](http://www.rabbitmq.com/tutorial-five-python.html)

    ruby receive_logs_topic.rb
    ruby emit_log_topic.rb

[Tutorial six: RPC](http://www.rabbitmq.com/tutorial-six-python.html)

    ruby rpc_server.rb
    ruby rpc_client.rb

To learn more, visit [Bunny documentation](http://rubybunny.info) site.
