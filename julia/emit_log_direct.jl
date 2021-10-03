using AMQPClient
const VIRTUALHOST = "/"
const HOST = "127.0.0.1"


function send()
    # 1. Create a connection to the localhost or 127.0.0.1 of virtualhost '/'
    connection(; virtualhost=VIRTUALHOST, host=HOST) do conn
        # 2. Create a channel to send messages
        channel(conn, AMQPClient.UNUSED_CHANNEL, true) do chan
            # 3. Declare exchange
            exchange = "direct_logs"
            exchange_declare(chan, exchange, EXCHANGE_TYPE_DIRECT)
            # 4. Get severity and message
            if length(Base.ARGS) >= 3
                severity = Base.ARGS[1]
                received = join(Base.ARGS[2:end], ' ')
            else
                severity = "info"
                received = "Hello World"
            end
            data = convert(Vector{UInt8}, codeunits(received))
            msg = Message(data, content_type="text/plain", delivery_mode=PERSISTENT)
            # 5. Publish the message
            basic_publish(chan, msg; exchange=exchange, routing_key=severity)
            println("Message sent: $received, Severity: $severity")
        end
    end
end

send()
