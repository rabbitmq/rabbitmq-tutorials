using AMQPClient
const VIRTUALHOST = "/"
const HOST = "127.0.0.1"


function receive()
    # 1. Create a connection to the localhost or 127.0.0.1 of virtualhost '/'
    connection(; virtualhost=VIRTUALHOST, host=HOST) do conn
        println("Connection established")
        # 2. Create a channel to send messages
        channel(conn, AMQPClient.UNUSED_CHANNEL, true) do chan
            # 3. Declare a queue
            println("Channel created")
            queue = "hello"
            success, queue_name, message_count, consumer_count = queue_declare(chan, queue)

            # 4. Setup to receive
            on_receive = (msg) -> begin
                # 4.1 Receive message is Vector{UInt8}
                data = String(msg.data)
                println("Received the message: $data")
                # 4.2 Acknowledge the message
                basic_ack(chan, msg.delivery_tag)
            end

            success, consumer_tag = basic_consume(chan, queue, on_receive)
            @assert success == true

            # 5. Run for-ever
            # listen to new messages
            while true
                sleep(1)
            end
        end
    end
end

# Don't exit on Ctrl-C
Base.exit_on_sigint(false)
try
    receive()
catch ex
    if ex isa InterruptException
        println("Interrupted")
    else
        println("Exception: $ex")
    end
end
