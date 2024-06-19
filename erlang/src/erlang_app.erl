%%%-------------------------------------------------------------------
%% @doc erlang public API
%% @end
%%%-------------------------------------------------------------------

-module(erlang_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    erlang_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
