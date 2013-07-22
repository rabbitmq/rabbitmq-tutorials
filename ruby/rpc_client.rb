#!/usr/bin/env ruby
# encoding: utf-8

require "bunny"

conn = Bunny.new
conn.start

ch   = conn.create_channel

class FibonacciClient
  attr_reader :reply_queue

  def initialize(ch, server_queue)
    @ch             = ch
    @x              = ch.default_exchange

    @server_queue   = server_queue
    @reply_queue    = ch.queue("", :exclusive => true)
  end

  def call(n)
    correlation_id = self.generate_uuid

    @x.publish(n.to_s,
      :routing_key    => @server_queue,
      :correlation_id => correlation_id,
      :reply_to       => @reply_queue.name)

    response = nil
    @reply_queue.subscribe(:block => true) do |delivery_info, properties, payload|
      if properties[:correlation_id] == correlation_id
        response = payload.to_i

        delivery_info.consumer.cancel
      end
    end

    response
  end

  protected

  def generate_uuid
    # very naive but good enough for code
    # examples
    "#{rand}#{rand}#{rand}"
  end
end


client   = FibonacciClient.new(ch, "rpc_queue")
puts " [x] Requesting fib(30)"
response = client.call(30)
puts " [.] Got #{response}"

ch.close
conn.close
