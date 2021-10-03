using AMQPClient
const VIRTUALHOST = "/"
const HOST = "127.0.0.1"


function receive()
    # 1. Create a connection to the localhost or 127.0.0.1 of virtualhost '/'
    connection(; virtualhost=VIRTUALHOST, host=HOST) do conn
        # 2. Create a channel to send messages
        channel(conn, AMQPClient.UNUSED_CHANNEL, true) do chan
            # 3. Declare a queue
            queue = "task_queue"
            success, queue_name, message_count, consumer_count = queue_declare(chan, queue, durable=true)
            println(" [*] Waiting for messages. To exit press CTRL+C")

            # 4. Setup function to receive message
            on_receive = (msg) -> begin
                data = String(msg.data)
                println("Received the message: $data")
                basic_ack(chan, msg.delivery_tag)
            end

            # 5. Configure Quality of Service
            basic_qos(chan, 0, 1, false)
            success, consumer_tag = basic_consume(chan, queue, on_receive)

            @assert success == true

            while true
                sleep(1)
            end
            # 5. Close the connection
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
