-module(rpc_client).
-export([start/1]).

-include_lib("amqp_client/include/amqp_client.hrl").

start(Argv) ->
    Num = case Argv of
             [] -> 10;
             [Arg] -> list_to_integer(Arg)
          end,
    io:format(" [x] Requesting fib(~p)~n", [Num]),
    Response = call(Num),
    io:format(" [.] Got ~p~n", [Response]),
    ok.

call(Num) ->
    {ok, Connection} =
        amqp_connection:start(#amqp_params_network{host = "localhost"}),
    {ok, Channel} = amqp_connection:open_channel(Connection),
    RequestQueue = <<"rpc_queue">>,
    CorrelationId = uuid:get_v4(),

    amqp_channel:call(Channel, #'queue.declare'{queue = RequestQueue}),

    #'queue.declare_ok'{queue = ReplyQueue} =
        amqp_channel:call(Channel, #'queue.declare'{exclusive = true}),

    Method = #'basic.consume'{queue = ReplyQueue, no_ack = true},
    amqp_channel:subscribe(Channel, Method, self()),

    amqp_channel:cast(Channel,
                      #'basic.publish'{
                        exchange = <<>>,
                        routing_key = RequestQueue},
                      #amqp_msg{
                        payload = integer_to_binary(Num),
                        props = #'P_basic'{
                            reply_to = ReplyQueue,
                            correlation_id = CorrelationId}
                        }),
    
    Response = wait_for_messages(CorrelationId),

    amqp_channel:close(Channel),
    amqp_connection:close(Connection),

    Response.

wait_for_messages(CorrelationId) ->
    receive
        {#'basic.deliver'{}, #amqp_msg{payload = Body, props = Props}} ->
            #'P_basic'{correlation_id = CorrId} = Props,
            if CorrelationId == CorrId ->
                binary_to_integer(Body);
            true ->
                -1
            end
    end.
