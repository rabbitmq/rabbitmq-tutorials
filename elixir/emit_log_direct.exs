{:ok, connection} = AMQP.Connection.open
{:ok, channel} = AMQP.Channel.open(connection)

{severities, raw_message, _} = 
  System.argv
  |> OptionParser.parse(strict: [info:    :boolean,
                                 warning: :boolean,
                                 error:   :boolean])
  |> case do
    {[], msg, _} -> {[info: true], msg, []}
    other -> other
  end

message = 
  case raw_message do
    []    -> "Hello World!"
    words -> Enum.join(words, " ")
  end

AMQP.Exchange.declare(channel, "direct_logs", :direct)

for {severity, true} <- severities do
  severity = severity |> to_string
  AMQP.Basic.publish(channel, "direct_logs", severity, message)
  IO.puts " [x] Sent '[#{severity}] #{message}'"
end

AMQP.Connection.close(connection)
