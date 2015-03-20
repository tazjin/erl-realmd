-module(realmd_protocol).
-behaviour(ranch_protocol).

-include("realmd_defines.hrl").

-export([start_link/4]).
-export([init/4]).

start_link(Ref, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.

init(Ref, Socket, Transport, _Opts = []) ->
    ok = ranch:accept_ack(Ref),
    loop(Socket, Transport).

loop(Socket, Transport) ->
    case Transport:recv(Socket, 1, 5000) of
        {ok, <<0>>} ->
            lager:debug("Dispatching client logon challenge handler"),
            handle_auth_logon_challenge(Socket, Transport),
            loop(Socket, Transport);
        _ ->
            lager:debug("closing socket"),
            ok = Transport:close(Socket)
    end.

handle_auth_packet() -> ok.

%% Protocol handlers

%%%%%%%%%%%%%%%%%%%%
%% Auth challenge %%
%%%%%%%%%%%%%%%%%%%%

handle_auth_logon_challenge(Socket, Transport) ->
    case Transport:recv(Socket, 7, 1000) of
        {ok, <<_Err, Len:16/little-integer, "WoW\0">>} ->
            lager:debug("Received auth challenge, length ~B", [Len]),
            {ok, Data} = Transport:recv(Socket, Len - 4, 1000),
            dispatch_auth_challenge(Socket, Transport, Data);
        _ ->
            lager:debug("Received invalid auth challenge"),
            ok = Transport:close(Socket)
    end.

dispatch_auth_challenge(Socket, Transport, Data) ->
    <<1, 12, 1, Build:16/little-integer,
      Platform:3/binary, "\0",
      OS:3/binary, "\0",
      Country:4/binary,
      _Timezone:32/integer,
      IP:4/binary,
      ILen:8/integer,
      I:ILen/binary,
      _Rest/binary>> = Data,
    LogonReply = <<0, 0, 0, 0>>
    lager:debug("ok ~p", [[Build, Platform, OS, Country, IP, ILen, I]]).
