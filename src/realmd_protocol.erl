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
    case Transport:recv(Socket, 0, 5000) of
        {ok, <<0,Data/binary>>} ->
            lager:debug("Received logon challenge, dispatching to handler"),
            handle_auth_logon_challenge(Socket, Transport, Data),
            loop(Socket, Transport);
%%            Transport:send(Socket, Data),
        _ ->
            lager:debug("closing socket"),
            ok = Transport:close(Socket)
    end.

%% Protocol handlers
handle_auth_logon_challenge(Socket, Transport, Data) ->
    lager:info("echoing ~p", [Data]),
    Transport:send(Socket, Data).
