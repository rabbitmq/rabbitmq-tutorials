#!/usr/bin/env ruby
# encoding: utf-8

require "bunny"

conn = Bunny.new
conn.start

ch       = conn.create_channel
x        = ch.direct("direct_logs")
severity = ARGV.shift || "info"
msg      = ARGV.empty? ? "Hello World!" : ARGV.join(" ")

x.publish(msg, :routing_key => severity)
puts " [x] Sent '#{msg}'"

sleep 0.5
conn.close
