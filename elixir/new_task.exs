{:ok, connection} = AMQP.Connection.open
{:ok, channel} = AMQP.Channel.open(connection)

AMQP.Queue.declare(channel, "task_queue", durable: true)

message = 
  case System.argv do
    []    -> "Hello World!"
    words -> Enum.join(words, " ")
  end

AMQP.Basic.publish(channel, "", "hello", message, persistent: true)
IO.puts " [x] Sent '#{message}'"

AMQP.Connection.close(connection)
