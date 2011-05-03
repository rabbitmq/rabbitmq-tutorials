#!/usr/bin/env ruby
# encoding: utf-8

require "amqp"

AMQP.start(:host => "localhost") do |connection|
  channel = AMQP::Channel.new(connection)
  queue   = channel.queue("task_queue", :durable => true)

  Signal.trap("INT") do
    connection.close do
      EM.stop { exit }
    end
  end

  puts " [*] Waiting for messages. To exit press CTRL+C"

  channel.prefetch(1)
  queue.subscribe do |header, body|
    puts " [x] Received #{body}"
    EM.add_timer(body.count(".")) do
      puts " [x] Done"
      header.ack
    end
  end
end
