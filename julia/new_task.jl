using AMQPClient
const VIRTUALHOST = "/"
const HOST = "127.0.0.1"

function send()
    # 1. Create a connection to the localhost or 127.0.0.1 of virtualhost '/'
    connection(; virtualhost=VIRTUALHOST, host=HOST) do conn
        # 2. Create a channel to send messages
        channel(conn, AMQPClient.UNUSED_CHANNEL, true) do chan
            queue = "task_queue"
            # 3. Configure the queue
            success, queue_name, message_count, consumer_count = queue_declare(chan, queue, durable=true)

            # 4. Prepare the message text
            if length(Base.ARGS) >= 1
                received = Base.ARGS[1]
            else
                received = "Hello World"
            end

            # 5. Prepare the payload
            data = convert(Vector{UInt8}, codeunits(received))
            msg = Message(data, content_type="text/plain", delivery_mode=PERSISTENT)

            # 6. Send the payload
            basic_publish(chan, msg; exchange="", routing_key=queue)
            println("Message sent: $received")
        end
    end
end

send()
