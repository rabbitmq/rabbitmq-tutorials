#!/usr/bin/env escript
%%! -pz ./_build/default/lib/amqp_client/ebin ./_build/default/lib/credentials_obfuscation/ebin ./_build/default/lib/jsx/ebin ./_build/default/lib/rabbit_common/ebin ./_build/default/lib/recon/ebin

-include_lib("amqp_client/include/amqp_client.hrl").

main(Argv) ->
    {ok, Connection} =
        amqp_connection:start(#amqp_params_network{host = "localhost"}),
    {ok, Channel} = amqp_connection:open_channel(Connection),

    amqp_channel:call(Channel, #'exchange.declare'{exchange = <<"topic_logs">>,
                                                   type = <<"topic">>}),

    {RoutingKey, Message} = case Argv of
                                [] ->
                                    {<<"anonymous.info">>, <<"Hello World!">>};
                                [R] ->
                                    {list_to_binary(R), <<"Hello World!">>};
                                [R | Msg] ->
                                    {list_to_binary(R), list_to_binary(string:join(Msg, " "))}
                            end,
    amqp_channel:cast(Channel,
                      #'basic.publish'{
                         exchange = <<"topic_logs">>,
                         routing_key = RoutingKey},
                      #amqp_msg{payload = Message}),
    io:format(" [x] Sent ~p:~p~n", [RoutingKey, Message]),
    ok = amqp_channel:close(Channel),
    ok = amqp_connection:close(Connection),
    ok.
