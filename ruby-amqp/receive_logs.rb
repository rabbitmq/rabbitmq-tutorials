#!/usr/bin/env ruby
# encoding: utf-8

require "amqp"

AMQP.start(:host => "localhost") do |connection|
  channel  = AMQP::Channel.new(connection)
  exchange = channel.fanout("logs")
  queue    = channel.queue("", :exclusive => true)

  queue.bind(exchange)

  Signal.trap("INT") do
    connection.close do
      EM.stop { exit }
    end
  end

  puts " [*] Waiting for logs. To exit press CTRL+C"

  queue.subscribe do |body|
    puts " [x] #{body}"
  end
end
