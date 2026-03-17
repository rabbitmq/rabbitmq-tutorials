{:ok, connection} = AMQP.Connection.open
{:ok, channel} = AMQP.Channel.open(connection)
AMQP.Queue.declare(channel, "hello", durable: true, arguments: [{"x-queue-type", :longstr, "quorum"}])
AMQP.Basic.publish(channel, "", "hello", "Hello World!")
IO.puts " [x] Sent 'Hello World!'"
AMQP.Connection.close(connection)
