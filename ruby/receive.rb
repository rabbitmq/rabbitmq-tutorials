#!/usr/bin/env ruby
# encoding: utf-8

require "amqp"

AMQP.start(:host => "localhost") do |connection|
  channel = AMQP::Channel.new(connection)
  queue   = channel.queue("hello")

  Signal.trap("INT") do
    connection.close do
      EM.stop { exit }
    end
  end

  puts " [*] Waiting for messages. To exit press CTRL+C"

  queue.subscribe do |body|
    puts " [x] Received #{body}"
  end
end
