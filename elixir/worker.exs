defmodule Worker do
  def wait_for_messages(channel) do
    receive do
      {:basic_deliver, payload, meta} ->
        IO.puts " [x] Received #{payload}"
        payload
        |> to_char_list
        |> Enum.count(fn x -> x == ?. end)
        |> Kernel.*(1000)
        |> :timer.sleep
        IO.puts " [x] Done."
        AMQP.Basic.ack(channel, meta.delivery_tag)

        wait_for_messages(channel)
    end
  end
end

{:ok, connection} = AMQP.Connection.open
{:ok, channel} = AMQP.Channel.open(connection)

AMQP.Queue.declare(channel, "task_queue", durable: true)
AMQP.Basic.qos(channel, prefetch_count: 1)

AMQP.Basic.consume(channel, "task_queue")
IO.puts " [*] Waiting for messages. To exit press CTRL+C, CTRL+C"

Worker.wait_for_messages(channel)
