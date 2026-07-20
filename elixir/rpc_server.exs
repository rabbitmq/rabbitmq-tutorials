defmodule FibServer do
  def fib(n), do: fib(n, 0, 1)
  defp fib(0, a, _b), do: a
  defp fib(n, a, b) when n > 0, do: fib(n - 1, b, a + b)

  def wait_for_messages(channel) do
    receive do
      {:basic_deliver, payload, meta} ->
        {n, _} = Integer.parse(payload)
        IO.puts " [.] fib(#{n})"
        response = fib(n)

        AMQP.Basic.publish(channel, "", meta.reply_to, "#{response}", correlation_id: meta.correlation_id)
        AMQP.Basic.ack(channel, meta.delivery_tag)

        wait_for_messages(channel)
    end
  end
end

{:ok, connection} = AMQP.Connection.open
{:ok, channel} = AMQP.Channel.open(connection)

AMQP.Queue.declare(channel, "rpc_queue", durable: true, arguments: [{"x-queue-type", :longstr, "quorum"}])

AMQP.Basic.qos(channel, prefetch_count: 1)

AMQP.Basic.consume(channel, "rpc_queue")

IO.puts " [x] Awaiting RPC requests"

FibServer.wait_for_messages(channel)

