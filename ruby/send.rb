#!/usr/bin/env ruby
require 'bunny'

connection = Bunny.new(automatically_recover: false)
connection.start

channel = connection.create_channel

# the default_exchange will route the message to a queue with the same name
# as the routing_key, so it will end in the receiver's hello queue
channel.default_exchange.publish('Hello World!', routing_key: 'hello')
puts " [x] Sent 'Hello World!'"

connection.close
