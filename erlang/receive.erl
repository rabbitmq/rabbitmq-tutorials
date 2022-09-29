#!/usr/bin/env escript
%%! -pz ./_build/default/lib/amqp_client/ebin ./_build/default/lib/credentials_obfuscation/ebin ./_build/default/lib/jsx/ebin ./_build/default/lib/rabbit_common/ebin ./_build/default/lib/recon/ebin 

-include_lib("amqp_client/include/amqp_client.hrl").

main(_) ->
    {ok, Connection} =
        amqp_connection:start(#amqp_params_network{host = "localhost", heartbeat = 30}),
    {ok, Channel} = amqp_connection:open_channel(Connection),

    amqp_channel:call(Channel, #'queue.declare'{queue = <<"hello">>}),
    io:format(" [*] Waiting for messages. To exit press CTRL+C~n"),

    Method = #'basic.consume'{queue = <<"hello">>, no_ack = true},
    amqp_channel:subscribe(Channel, Method, self()),
    loop(Channel).

loop(Channel) ->
    receive
        #'basic.consume_ok'{} ->
            io:format(" [x] Saw basic.consume_ok~n"),
            loop(Channel);
        {#'basic.deliver'{}, #amqp_msg{payload = Body}} ->
            io:format(" [x] Received ~p~n", [Body]),
            loop(Channel)
    end.
