defmodule ReceiveLogsTopic do
  def wait_for_messages(channel) do
    receive do
      {:basic_deliver, payload, meta} ->
        IO.puts " [x] Received [#{meta.routing_key}] #{payload}"
        
        wait_for_messages(channel)
    end
  end
end
  
{:ok, connection} = AMQP.Connection.open
{:ok, channel} = AMQP.Channel.open(connection)

AMQP.Exchange.declare(channel, "topic_logs", :topic)

{:ok, %{queue: queue_name}} = AMQP.Queue.declare(channel, "", exclusive: true)

if length(System.argv) == 0 do
  IO.puts "Usage: mix run receive_logs_topic.exs [binding_key]..."
  System.halt(1)
end
for binding_key <- System.argv do
  AMQP.Queue.bind(channel, queue_name, "topic_logs", routing_key: binding_key)
end

AMQP.Basic.consume(channel, queue_name, nil, no_ack: true)

IO.puts " [*] Waiting for messages. To exist press CTRL+C, CTRL+C"

ReceiveLogsTopic.wait_for_messages(channel)
