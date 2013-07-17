# Ruby (amqp gem) code for RabbitMQ tutorials

Here you can find Ruby code examples from
[RabbitMQ tutorials](http://www.rabbitmq.com/getstarted.html).

## Requirements

If you use Microsoft Windows, we highly recommend you to use [JRuby](http://jruby.org).

To run this code you need [amqp gem](http://rubyamqp.info).

You can install it via RubyGems. On Linux, Mac OS X and *BSD systems:

    gem install amqp --version ">= 1.0.2"

On Windows:

    jruby.exe --1.9 -S gem install amqp --version ">= 1.0.2"

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

To learn more, visit [Ruby AMQP gem documentation](http://rubyamqp.info) site.
