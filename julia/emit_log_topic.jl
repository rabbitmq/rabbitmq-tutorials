using AMQPClient
const VIRTUALHOST = "/"
const HOST = "127.0.0.1"

function send()
    # 1. Create a connection to the localhost or 127.0.0.1 of virtualhost '/'
    connection(; virtualhost=VIRTUALHOST, host=HOST) do conn
        # 2. Create a channel to send messages
        channel(conn, AMQPClient.UNUSED_CHANNEL, true) do chan
            # 3. Declare Exchange
            exchange = "topic_logs"
            exchange_topic = "topic"
            exchange_declare(chan, exchange, EXCHANGE_TYPE_TOPIC)
            # 4. Get input data
            if length(Base.ARGS) >= 2
                routing_key = Base.ARGS[1]
                received = join(Base.ARGS[2:end], ' ')
            else
                routing_key = "info"
                received = "Hello World"
            end
            # 5. Prepare and send data
            data = convert(Vector{UInt8}, codeunits(received))
            msg = Message(data, content_type="text/plain", delivery_mode=PERSISTENT)
            basic_publish(chan, msg; exchange=exchange, routing_key=routing_key)
            println("Message sent: $received, routing key: $routing_key")
        end
    end
end

send()
