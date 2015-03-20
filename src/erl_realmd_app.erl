-module(erl_realmd_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, _} = ranch:start_listener(realmd, 1, ranch_tcp, [{port, 3724}],
                                   realmd_protocol, []),
    erl_realmd_sup:start_link().

stop(_State) ->
    ok.
