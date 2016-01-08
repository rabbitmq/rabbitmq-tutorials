defmodule FibonacciRpcClient do
  def wait_for_messages(_channel, correlation_id) do
    receive do
      {:basic_deliver, payload, %{correlation_id: ^correlation_id}} ->
        {n, _} = Integer.parse(payload)
        n
    end
  end
  def call(n) do
    {:ok, connection} = AMQP.Connection.open
    {:ok, channel} = AMQP.Channel.open(connection)

    {:ok, %{queue: queue_name}} = AMQP.Queue.declare(channel, "", exclusive: true)
    AMQP.Basic.consume(channel, queue_name, nil, no_ack: true)
    correlation_id = :erlang.unique_integer |> :erlang.integer_to_binary |> Base.encode64
    request = to_string(n)
    AMQP.Basic.publish(channel, "", "rpc_queue", request, reply_to: queue_name, correlation_id: correlation_id)

    FibonacciRpcClient.wait_for_messages(channel, correlation_id)
  end
end

num =
  case System.argv do
    []    -> 30
    param ->
      {x, _} =
        param
        |> Enum.join(" ")
        |> Integer.parse
      x
  end

IO.puts " [x] Requesting fib(#{num})"
response = FibonacciRpcClient.call(num)
IO.puts " [.] Got #{response}"
