using AMQPClient
const VIRTUALHOST = "/"
const HOST = "127.0.0.1"
using JSON


function fib(n)
    if n == 0
        return 0
    elseif n == 1
        return 1
    else
        return fib(n - 1) + fib(n - 2)
    end
end

function receive()
    # 1. Create a connection to the localhost or 127.0.0.1 of virtualhost '/'
    connection(; virtualhost=VIRTUALHOST, host=HOST) do conn
        println("Connection established")
        # 2. Create a channel to send messages
        channel(conn, AMQPClient.UNUSED_CHANNEL, true) do chan
            # 3. Declare a queue
            println("Channel created")
            queue = "rpc_queue"
            success, queue_name, message_count, consumer_count = queue_declare(chan, queue; durable=true)

            # 4. Receiver requests and dispatch response
            on_receive = (msg) -> begin
                data = String(msg.data)
                # 4.1 Deserialize
                number = JSON.parse(data)
                println("Received requets to calculate fib[$number]")
                try
                    output = fib(number)
                    res = convert(Vector{UInt8}, codeunits(JSON.json(output)))
                    # 4.2 Ack the received request
                    basic_ack(chan, msg.delivery_tag)
                    # 4.3 Get the reply details from the message
                    reply_to = convert(String, msg.properties[:reply_to])
                    corr_id = convert(String, msg.properties[:correlation_id])
                    resp = Message(res, content_type="text/plain", delivery_mode=PERSISTENT, correlation_id=corr_id)
                    # Sent the response
                    basic_publish(chan, resp; exchange="", routing_key=reply_to)
                    println("Sent response for $number as $output")
                catch err
                    println(err)
                end
            end


            # 5. Consume requests for-ever
            success, consumer_tag = basic_consume(chan, queue, on_receive)

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
