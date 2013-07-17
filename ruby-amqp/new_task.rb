#!/usr/bin/env ruby
# encoding: utf-8

require "amqp"

AMQP.start(:host => "localhost") do |connection|
  channel = AMQP::Channel.new(connection)
  queue   = channel.queue("task_queue", :durable => true)
  message = ARGV.empty? ? "Hello World!" : ARGV.join(" ")

  AMQP::Exchange.default.publish(message, :routing_key => queue.name, :persistent => true)
  puts " [x] Sent #{message}"

  EM.add_timer(0.5) do
    connection.close do
      EM.stop { exit }
    end
  end
end
