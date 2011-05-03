#!/usr/bin/env ruby
# encoding: utf-8

require "amqp"

AMQP.start(:host => "localhost") do |connection|
  channel  = AMQP::Channel.new(connection)
  exchange = channel.topic("topic_logs")
  severity = ARGV[0] || "anonymous.info"
  message  = (ARGV[1..-1] || ["Hello World!"]).join(" ")

  exchange.publish(message, :routing_key => severity)
  puts " [x] Sent #{severity}:#{message}"

  EM.add_timer(0.5) do
    connection.close do
      EM.stop { exit }
    end
  end
end
