-module(rpc_server).
-export([start/0]).

-include_lib("amqp_client/include/amqp_client.hrl").

start() ->
    {ok, Connection} =
        amqp_connection:start(#amqp_params_network{host = "localhost", heartbeat = 30}),
    {ok, Channel} = amqp_connection:open_channel(Connection),

    amqp_channel:call(Channel, #'queue.declare'{queue = <<"rpc_queue">>}),
    io:format(" [*] Waiting for messages. To exit press CTRL+C~n"),

    amqp_channel:call(Channel, #'basic.qos'{prefetch_count = 1}),

    Method = #'basic.consume'{queue = <<"rpc_queue">>},
    amqp_channel:subscribe(Channel, Method, self()),
    loop(Channel).

loop(Channel) ->
    receive
        {#'basic.deliver'{delivery_tag = DeliveryTag}, #amqp_msg{payload = Body, props = Props}} ->
            #'P_basic'{reply_to = ReplyTo, correlation_id = CorrelationId} = Props,
            Num = binary_to_integer(Body),
            io:format(" [.] fib(~p)~n", [Num]),
            Response = fib(Num),

            amqp_channel:cast(Channel,
                    #'basic.publish'{
                        exchange = <<>>,
                        routing_key = ReplyTo},
                    #amqp_msg{
                        payload = integer_to_binary(Response),
                        props = #'P_basic'{
                            correlation_id = CorrelationId}
                        }),

            amqp_channel:cast(Channel,
                    #'basic.ack'{
                        delivery_tag = DeliveryTag
                    }),

            loop(Channel)
    end.

fib(0) -> 0;
fib(1) -> 1;
fib(N) when N > 1 -> fib(N-1) + fib(N-2);
fib(N) when N =< 0 -> -1.