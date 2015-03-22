-module(realmd_auth_proof).
-export([handle_auth_proof/3]).

-include("realmd_defines.hrl").

hexstring(Binary) when is_binary(Binary) ->
    lists:flatten(lists:map(
        fun(X) -> io_lib:format("~2.16.0b", [X]) end, 
        binary_to_list(Binary))).

handle_auth_proof(Socket, Transport, S) ->
    lager:debug("entered await_auth_proof"),
    case Transport:recv(Socket, 74, 5000) of
        {ok, <<ClientPub:256?UINT, M1:160?UINT, _Rest/binary>>} ->
            lager:debug("Received auth proof (pub: ~p, m1: ~p)", [ClientPub, M1]),
            SN = srp6:proof(S, ClientPub, <<"TEST">>),
            lager:debug("Calculated session proof ~d", [SN#session.session_proof]),
            ok = Transport:send(Socket, <<1, 0, (SN#session.session_proof):160?UINT,
                                          0, 0:64, 0:16>>),
            SN;
%            ok = Transport:send(Socket, <<0, 0, ?ce_server_full>>);
        _ ->
            lager:debug("Received invalid auth proof"),
            ok = Transport:close(Socket)
    end.

