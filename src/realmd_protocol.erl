-module(realmd_protocol).
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/4]).

start_link(Ref, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.

init(Ref, Socket, Transport, _Opts = []) ->
    ok = ranch:accept_ack(Ref),
    loop(Socket, Transport, no_session).

loop(Socket, Transport, Session) ->
    case Transport:recv(Socket, 1, 1500) of
        {ok, <<0>>} ->
            lager:debug("Dispatching client logon challenge handler"),
            S = realmd_auth_challenge:handle_auth_challenge(Socket, Transport),
            loop(Socket, Transport, S);
        {ok, <<1>>} ->
            lager:debug("Dispatching client proof handler"),
            S = realmd_auth_proof:handle_auth_proof(Socket, Transport, Session),
            loop(Socket, Transport, S);
        {ok, Unknown} ->
            lager:debug("Received unknown CMD ~p", [Unknown]),
            ok = Transport:close(Socket);
        {error, closed} ->
            lager:error("Connection closed by client");
        {error, timeout} ->
            lager:error("Connection timed out")
    end.
