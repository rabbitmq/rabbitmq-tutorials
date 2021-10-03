using AMQPClient
const VIRTUALHOST = "/"
const HOST = "127.0.0.1"


function receive()
    # 1. Create a connection to the localhost or 127.0.0.1 of virtualhost '/'
    connection(; virtualhost=VIRTUALHOST, host=HOST) do conn
        # 2. Create a channel to send messages
        channel(conn, AMQPClient.UNUSED_CHANNEL, true) do chan
            # 3. Declare a exchange
            exchange = "logs"
            exchange_declare(chan, exchange, EXCHANGE_TYPE_FANOUT)
            result, queue, _, _ = queue_declare(chan, "", durable=true)
            # 4. Bind the queue
            queue_bind(chan, queue, exchange, EXCHANGE_TYPE_FANOUT)

            println(" [*] Waiting for messages. To exit press CTRL+C")
            on_receive = (msg) -> begin
                data = String(msg.data)
                println("Received the message: $data")
                basic_ack(chan, msg.delivery_tag)
            end

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
