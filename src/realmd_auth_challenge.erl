-module(realmd_auth_challenge).
-export([handle_auth_challenge/2]).

-include("realmd_defines.hrl").

%%%%%%%%%%%%%%%%%%%%
%% Auth challenge %%
%%   First step   %%
%%%%%%%%%%%%%%%%%%%%

handle_auth_challenge(Socket, Transport) ->
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
    <<1, 12, 1, _Build:16/little-integer,
      _Platform:3/binary, "\0",
      OS:3/binary, "\0",
      Country:4/binary,
      _Timezone:32/integer,
      IP:4/binary,
      ILen:8/integer,
      I:ILen/binary,
      _Rest/binary>> = Data,
    lager:debug("AUTH_CHALLENGE from ~p with OS ~p, Country ~p, User ~p",
                [IP, OS, Country, I]),
    % Calculate SRP6 public key
    case realmd_user:get_srp_ingredients(I) of
        {ok, {hash, Hash}} ->
            lager:debug("Found account, responding with server challenge"),
            auth_challenge_respond_success(Transport, Socket, Hash);
        {error, banned} ->
            lager:debug("Rejecting banned account ~p from ~p", [I, IP]),
            Transport:send(Socket, <<0, 0, ?ce_account_closed>>);
        {error, notfound} ->
            lager:debug("Unknown account ~p from ~p", [I, IP]),
            Transport:send(Socket, <<0, 0, ?ce_no_account>>)
    end.

%% Respond with success (user exists!)
auth_challenge_respond_success(Transport, Socket, Hash) ->
    S = srp6:challenge(Hash),
    Unknown   = crypto:strong_rand_bytes(16), % This is unknown but different every time
    Response  = <<0, 0, ?ce_success,
                  (S#session.public):256?UINT,
                  1, 7, 32,
                  ?safe_prime:256?UINT,
                  (S#session.salt):256?UINT,
                  Unknown:16/binary, 0>>,
    lager:debug("Responding ~p", [Response]),
    Transport:send(Socket, Response),
    S.
