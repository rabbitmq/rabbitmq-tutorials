#!/usr/bin/env escript
%%! -pz ./amqp_client ./rabbit_common ./amqp_client/ebin ./rabbit_common/ebin

-include_lib("amqp_client/include/amqp_client.hrl").

main(_) ->
    {ok, Connection} = amqp_connection:start(network,
                                             #amqp_params{host = "localhost"}),
    {ok, Channel} = amqp_connection:open_channel(Connection),

    amqp_channel:call(Channel, #'exchange.declare'{exchange = <<"logs">>,
                                                   type = <<"fanout">>}),

    #'queue.declare_ok'{queue = Queue} =
        amqp_channel:call(Channel, #'queue.declare'{exclusive = true}),

    amqp_channel:call(Channel, #'queue.bind'{exchange = <<"logs">>,
                                             queue = Queue}),

    io:format(" [*] Waiting for logs. To exit press CTRL+C\n"),

    amqp_channel:subscribe(Channel, #'basic.consume'{queue = Queue,
                                                     no_ack = true}, self()),
    receive
        #'basic.consume_ok'{} -> ok
    end,
    loop(Channel).

loop(Channel) ->
    receive
        {#'basic.deliver'{}, #amqp_msg{payload = Body}} ->
            io:format(" [x] ~p\n", [Body]),
            loop(Channel)
    end.
