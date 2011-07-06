#!/usr/bin/env ruby
# encoding: utf-8

require "amqp"

AMQP.start(:host => "localhost") do |connection|
  channel = AMQP::Channel.new(connection)
  queue   = channel.queue("hello")

  channel.default_exchange.publish("Hello World!", :routing_key => queue.name)
  puts " [x] Sent 'Hello World!'"

  EM.add_timer(0.5) do
    connection.close do
      EM.stop { exit }
    end
  end
end
