using AMQPClient
const VIRTUALHOST ="/"
const HOST = "127.0.0.1"


function send(message)
    # 1. Create a connection to the localhost or 127.0.0.1 of virtualhost '/'
    connection(; virtualhost=VIRTUALHOST, host=HOST) do conn
        # 2. Create a channel to send messages
        channel(conn, AMQPClient.UNUSED_CHANNEL, true) do chan
            # 3. Declare a queue
            success, queue_name, message_count, consumer_count = queue_declare(chan, "hello")
            # 4.1 Create a message, AMQPCleint only accepts message in UInt8
            data = convert(Vector{UInt8}, codeunits(message))
            msg = Message(data, content_type="text/plain", delivery_mode=PERSISTENT)
            # 4.2 Send a message
            basic_publish(chan, msg; exchange="", routing_key="hello")
            println("Message sent: $message")
            # 5. Auto-closes the channel and connection
        end
    end
end

send("Hello World!")
