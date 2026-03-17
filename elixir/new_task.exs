{:ok, connection} = AMQP.Connection.open
{:ok, channel} = AMQP.Channel.open(connection)

AMQP.Queue.declare(channel, "task_queue", durable: true, arguments: [{"x-queue-type", :longstr, "quorum"}])

message = 
  case System.argv do
    []    -> "Hello World!"
    words -> Enum.join(words, " ")
  end

AMQP.Basic.publish(channel, "", "task_queue", message, persistent: true)
IO.puts " [x] Sent '#{message}'"

AMQP.Connection.close(connection)
