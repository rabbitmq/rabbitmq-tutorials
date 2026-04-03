#!/usr/bin/env ruby
require 'bunny'

MESSAGE_COUNT = 50_000

def publish_messages_individually(connection)
  channel = connection.create_channel
  queue = channel.queue('', exclusive: true)

  channel.confirm_select(tracking: true)

  start_time = Time.now
  MESSAGE_COUNT.times do |i|
    body = i.to_s
    channel.basic_publish(body, '', queue.name)
  end
  
  # Wait for any remaining confirmations
  channel.wait_for_confirms
  end_time = Time.now

  puts "Published #{MESSAGE_COUNT} messages individually in #{((end_time - start_time) * 1000).to_i} ms"
ensure
  channel.close if channel && channel.open?
end

def publish_messages_in_batch(connection)
  channel = connection.create_channel
  queue = channel.queue('', exclusive: true)

  channel.confirm_select(tracking: true)

  batch_size = 1000
  start_time = Time.now

  (0...MESSAGE_COUNT).each_slice(batch_size) do |batch|
    messages = batch.map { |i| i.to_s }
    channel.basic_publish_batch(messages, '', queue.name)
  end
  
  # Wait for any remaining confirmations
  channel.wait_for_confirms
  end_time = Time.now

  puts "Published #{MESSAGE_COUNT} messages in batch in #{((end_time - start_time) * 1000).to_i} ms"
ensure
  channel.close if channel && channel.open?
end

def handle_publish_confirms_asynchronously(connection)
  channel = connection.create_channel
  queue = channel.queue('', exclusive: true)

  outstanding_confirms = {}
  # A mutex is necessary because the confirm callbacks are executed in a separate thread
  confirms_mutex = Mutex.new

  channel.confirm_select do |delivery_tag, multiple, nack|
    confirms_mutex.synchronize do
      if multiple
        outstanding_confirms.reject! { |k, _| k <= delivery_tag }
      else
        outstanding_confirms.delete(delivery_tag)
      end
    end
    if nack
      puts "Message with delivery tag #{delivery_tag} was nacked!"
    end
  end

  start_time = Time.now
  MESSAGE_COUNT.times do |i|
    body = i.to_s
    seq_no = channel.next_publish_seq_no
    confirms_mutex.synchronize do
      outstanding_confirms[seq_no] = body
    end
    channel.basic_publish(body, '', queue.name)
  end

  # Wait for any remaining confirmations
  channel.wait_for_confirms
  end_time = Time.now

  puts "Published #{MESSAGE_COUNT} messages and handled confirms asynchronously in #{((end_time - start_time) * 1000).to_i} ms"
ensure
  channel.close if channel && channel.open?
end

begin
  connection = Bunny.new
  connection.start

  publish_messages_individually(connection)
  publish_messages_in_batch(connection)
  handle_publish_confirms_asynchronously(connection)
rescue Interrupt => _
  connection.close
  exit(0)
ensure
  connection.close if connection
end
