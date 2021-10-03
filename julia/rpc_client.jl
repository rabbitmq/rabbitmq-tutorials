using AMQPClient
const VIRTUALHOST = "/"
const HOST = "127.0.0.1"
using UUIDs
using JSON

function send(n)
    # 1. Create Connection
    connection(; virtualhost=VIRTUALHOST, host=HOST) do conn
        println("Created Connection")
        # 2. Create a channel to send messages
        channel(conn, AMQPClient.UNUSED_CHANNEL, true) do chan
            println("Created Channel")
            # 3. Create queue for response
            success, queue_name, message_count, consumer_count = queue_declare(chan, "", exclusive=true)
            corr_id = string(uuid4())

            # 4. Receive the response
            on_receive = (msg) -> begin
                try
                    response = String(msg.data)
                    msg_corr_id = convert(String, msg.properties[:correlation_id])
                    println(msg_corr_id)
                    if msg_corr_id == corr_id
                        println("Received RPC response: $response")
                    end
                    basic_ack(chan, msg.delivery_tag)
                catch err
                    println(err)
                end
            end

            success, consumer_tag = basic_consume(chan, queue_name, on_receive)

            # 5. Send RPC Request in JSON encoding since integer is disallowed
            data = convert(Vector{UInt8}, codeunits(JSON.json(n)))
            msg = Message(data, content_type="text/plain", delivery_mode=PERSISTENT, correlation_id=corr_id,
                          reply_to=queue_name)
            routing_key = "rpc_queue"
            basic_publish(chan, msg; exchange="", routing_key=routing_key)
            println("RPC Request sent: $n, routing_key: $queue_name")

            # Listen for ever
            while true
                sleep(1)
            end

        end
    end
end

# Don't exit on Ctrl-C
Base.exit_on_sigint(false)
try
    send(30)
catch ex
    if ex isa InterruptException
        println("Interrupted")
    else
        println("Exception: $ex")
    end
end
