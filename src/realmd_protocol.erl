-module(realmd_protocol).
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/4]).

start_link(Ref, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.

init(Ref, Socket, Transport, _Opts = []) ->
    ok = ranch:accept_ack(Ref),
    loop(Socket, Transport).

loop(Socket, Transport) ->
    case Transport:recv(Socket, 1, 1500) of
        {ok, <<0>>} ->
            lager:debug("Dispatching client logon challenge handler"),
            realmd_auth_challenge:handle_auth_challenge(Socket, Transport),
            loop(Socket, Transport);
        {ok, Unknown} ->
            lager:debug("Received unknown CMD ~p", [Unknown]),
            ok = Transport:close(Socket),
            loop(Socket, Transport);
        {error, closed} ->
            lager:error("Connection closed by client");
        {error, timeout} ->
            lager:error("Connection timed out")
    end.
