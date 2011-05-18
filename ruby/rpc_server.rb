#!/usr/bin/env ruby
# encoding: utf-8

require "amqp"

def fib(n)
  return n if n == 0 || n == 1
  return fib(n - 1) + fib(n - 2)
end

AMQP.start(:host => "localhost") do |connection|
  channel = AMQP::Channel.new(connection)
  queue   = channel.queue("rpc_queue")

  channel.prefetch(1)

  queue.subscribe(:ack => true) do |header, body|
    n = body.to_i

    puts " [.] fib(#{n})"
    response = fib(n)

    AMQP::Exchange.default.publish(response.to_s, :routing_key => header.reply_to, :correlation_id => header.correlation_id)
    header.ack
  end

  puts " [x] Awaiting RPC requests"
end
