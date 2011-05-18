# Ruby code for RabbitMQ tutorials

Here you can find Ruby code examples from [RabbitMQ tutorials](http://www.rabbitmq.com/getstarted.html).

## Requirements

To run this code you need [amqp gem](http://bit.ly/itcpVv) version 0.8 (currently available as a prerelease version). This code won't work with earlier versions! You can install it via RubyGems thusly:

    gem install amqp --pre

## Code

[Tutorial one: "Hello World!"](http://www.rabbitmq.com/tutorial-one-ruby.html):

    ruby send.rb
    ruby receive.rb

[Tutorial two: Work Queues](http://www.rabbitmq.com/tutorial-two-ruby.html):

    ruby new_task.rb
    ruby worker.rb

[Tutorial three: Publish/Subscribe](http://www.rabbitmq.com/tutorial-three-ruby.html)

    ruby receive_logs.rb
    ruby emit_log.rb

To learn more, visit [Ruby AMQP gem documentation](http://bit.ly/mDm1JE) site.