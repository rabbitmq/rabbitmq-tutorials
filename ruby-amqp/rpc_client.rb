#!/usr/bin/env ruby
# encoding: utf-8

# Note: This is just proof of concept. For
# real-world usage, you are strongly advised
# to use https://github.com/ruby-amqp/rpc
# or some other RPC library.

require "amqp"

class FibonacciRpcClient
  def initialize
    subscribe_to_callback_queue
  end

  def connection
    @connection ||= AMQP.connect(:host => "localhost")
  end

  def channel
    @channel ||= AMQP::Channel.new(self.connection)
  end

  def callback_queue
    @callback_queue ||= self.channel.queue("", :exclusive => true)
  end

  def requests
    @requests ||= Hash.new
  end

  def call(n, &block)
    corr_id = rand(10_000_000).to_s
    self.requests[corr_id] = nil
    self.callback_queue.append_callback(:declare) do
      self.channel.default_exchange.publish(n.to_s, :routing_key => "rpc_queue", :reply_to => self.callback_queue.name, :correlation_id => corr_id)

      EM.add_periodic_timer(0.1) do
        # p self.requests
        if result = self.requests[corr_id]
          block.call(result.to_i)
          EM.stop
        end
      end
    end
  end

  private
  def subscribe_to_callback_queue
    self.callback_queue.subscribe do |header, body|
      corr_id = header.correlation_id
      unless self.requests[corr_id]
        self.requests[corr_id] = body
      end
    end
  end
end

EM.run do
  fibonacci_rpc = FibonacciRpcClient.new()

  puts " [x] Requesting fib(30)"
  fibonacci_rpc.call(30) do |response|
    puts " [.] Got #{response}"
  end
end
