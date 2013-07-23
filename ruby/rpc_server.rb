#!/usr/bin/env ruby
# encoding: utf-8

require "bunny"

conn = Bunny.new
conn.start

ch   = conn.create_channel

class FibonacciServer

  def initialize(ch)
    @ch = ch
  end

  def start(queue_name)
    @q = @ch.queue(queue_name)
    @x = @ch.default_exchange

    @q.subscribe(:block => true) do |delivery_info, properties, payload|
      n = payload.to_i
      r = self.class.fib(n)

      puts " [.] fib(#{n})"

      @x.publish(r.to_s, :routing_key => properties.reply_to, :correlation_id => properties.correlation_id)
    end
  end


  def self.fib(n)
    case n
    when 0 then 0
    when 1 then 1
    else
      fib(n - 1) + fib(n - 2)
    end
  end
end

begin
  server = FibonacciServer.new(ch)
  " [x] Awaiting RPC requests"
  server.start("rpc_queue")
rescue Interrupt => _
  ch.close
  conn.close

  exit(0)
end
