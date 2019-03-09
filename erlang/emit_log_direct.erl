#!/usr/bin/env escript
%%! -pz ./deps/amqp_client/ebin ./deps/rabbit_common/ebin ./deps/amqp_client/ebin ./deps/rabbit_common/ebin ./deps/recon/ebin ./deps/lager/ebin ./deps/goldrush/ebin ./deps/jsx/ebin ./deps/ranch/ebin

-include_lib("amqp_client/include/amqp_client.hrl").

main(Argv) ->
    {ok, Connection} =
        amqp_connection:start(#amqp_params_network{host = "localhost"}),
    {ok, Channel} = amqp_connection:open_channel(Connection),

    amqp_channel:call(Channel, #'exchange.declare'{exchange = <<"direct_logs">>,
                                                   type = <<"direct">>}),

    {Severity, Message} = case Argv of
                              [] ->
                                  {<<"info">>, <<"Hello World!">>};
                              [S] ->
                                  {list_to_binary(S), <<"Hello World!">>};
                              [S | Msg] ->
                                  {list_to_binary(S), list_to_binary(string:join(Msg, " "))}
                          end,
    amqp_channel:cast(Channel,
                      #'basic.publish'{
                        exchange = <<"direct_logs">>,
                        routing_key = Severity},
                      #amqp_msg{payload = Message}),
    io:format(" [x] Sent ~p:~p~n", [Severity, Message]),
    ok = amqp_channel:close(Channel),
    ok = amqp_connection:close(Connection),
    ok.
