using AMQPClient
const VIRTUALHOST = "/"
const HOST = "127.0.0.1"

function send()
    # 1. Create a connection to the localhost or 127.0.0.1 of virtualhost '/'
    connection(; virtualhost=VIRTUALHOST, host=HOST) do conn
        # 2. Create a channel to send messages
        channel(conn, AMQPClient.UNUSED_CHANNEL, true) do chan
            exchange = "logs"
            # 3. Declare the exchange
            exchange_declare(chan, exchange, EXCHANGE_TYPE_FANOUT)
            if length(Base.ARGS) >= 1
                received = join(Base.ARGS, ' ')
            else
                received = "info: Hello World"
            end

            data = convert(Vector{UInt8}, codeunits(received))
            msg = Message(data, content_type="text/plain", delivery_mode=PERSISTENT)

            # 4. Publish message
            basic_publish(chan, msg; exchange=exchange, routing_key="")
            println("Message sent: $received")
        end
    end
end

send()
