#!/usr/bin/env ruby
# encoding: utf-8

require "bunny"

conn = Bunny.new
conn.start

ch   = conn.create_channel
q    = ch.queue("hello")

begin
  puts " [*] Waiting for messages in #{q.name}. To exit press CTRL+C"
  q.subscribe(:block => true) do |delivery_info, properties, body|
    puts " [x] Received #{body}"
  end
rescue Interrupt => _
  puts " [x] Shutting down..."

  conn.close
end
