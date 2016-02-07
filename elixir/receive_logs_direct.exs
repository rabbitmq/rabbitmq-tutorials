defmodule ReceiveLogsDirect do
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

{severities, _, _} = 
  System.argv
  |> OptionParser.parse(strict: [info:    :boolean,
                                 warning: :boolean,
                                 error:   :boolean])

AMQP.Exchange.declare(channel, "direct_logs", :direct)

{:ok, %{queue: queue_name}} = AMQP.Queue.declare(channel, "", exclusive: true)

for {severity, true} <- severities do
  binding_key = severity |> to_string
  AMQP.Queue.bind(channel, queue_name, "direct_logs", routing_key: binding_key)
end

AMQP.Basic.consume(channel, queue_name, nil, no_ack: true)

IO.puts " [*] Waiting for messages. To exist press CTRL+C, CTRL+C"


ReceiveLogsDirect.wait_for_messages(channel)
