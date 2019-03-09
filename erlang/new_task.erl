#!/usr/bin/env escript
%%! -pz ./deps/amqp_client/ebin ./deps/rabbit_common/ebin ./deps/amqp_client/ebin ./deps/rabbit_common/ebin ./deps/recon/ebin ./deps/lager/ebin ./deps/goldrush/ebin ./deps/jsx/ebin ./deps/ranch/ebin

-include_lib("amqp_client/include/amqp_client.hrl").

main(Argv) ->
    {ok, Connection} =
        amqp_connection:start(#amqp_params_network{host = "localhost"}),
    {ok, Channel} = amqp_connection:open_channel(Connection),

    amqp_channel:call(Channel, #'queue.declare'{queue = <<"task_queue">>,
                                                durable = true}),

    Message = case Argv of
                  [] -> <<"Hello World!">>;
                  Msg -> list_to_binary(string:join(Msg, " "))
              end,
    amqp_channel:cast(Channel,
                      #'basic.publish'{
                        exchange = <<"">>,
                        routing_key = <<"task_queue">>},
                      #amqp_msg{props = #'P_basic'{delivery_mode = 2},
                                payload = Message}),
    io:format(" [x] Sent ~p~n", [Message]),
    ok = amqp_channel:close(Channel),
    ok = amqp_connection:close(Connection),
    ok.
