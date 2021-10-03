using AMQPClient
const VIRTUALHOST = "/"
const HOST = "127.0.0.1"


function receive()
    # 1. Create a connection to the localhost or 127.0.0.1 of virtualhost '/'
    connection(; virtualhost=VIRTUALHOST, host=HOST) do conn
        # 2. Create a channel to send messages
        channel(conn, AMQPClient.UNUSED_CHANNEL, true) do chan
            # 3. Declare a exchange
            exchange = "direct_logs"
            exchange_declare(chan, exchange, EXCHANGE_TYPE_DIRECT)

            result, queue_name, _, _  = queue_declare(chan, "", exclusive=true)

            # 4. Receive queues to bind
            if length(Base.ARGS) <= 0
                println(Base.stdout, "Usage: [info] [warning] [error]\n")
                Base.exit(1)
            end

            # 4.1 Bind queues
            for severity in Base.ARGS[1:end]
                queue_bind(chan, queue_name, exchange,
                           severity)
            end

            println(" [*] Waiting for messages. To exit press CTRL+C")
            on_receive = (msg) -> begin
                data = String(msg.data)
                println("Received the message: $data")
                basic_ack(chan, msg.delivery_tag)
            end

            success, consumer_tag = basic_consume(chan, queue_name, on_receive)

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
