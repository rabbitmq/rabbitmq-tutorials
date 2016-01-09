{:ok, connection} = AMQP.Connection.open
{:ok, channel} = AMQP.Channel.open(connection)

{topic, message} = 
  System.argv
  |> case do
    []            -> {"anonymous.info", "Hello World!"}
    [message]     -> {"anonymous.info", message}
    [topic|words] -> {topic, Enum.join(words, " ")}
  end

AMQP.Exchange.declare(channel, "topic_logs", :topic)

AMQP.Basic.publish(channel, "topic_logs", topic, message)
IO.puts " [x] Sent '[#{topic}] #{message}'"

AMQP.Connection.close(connection)
