-module(srp6).
-export([challenge/1, proof/3]).
-compile(export_all).
-include("realmd_defines.hrl").


%% @spec sha(binary()) -> int().
sha_int(Data) ->
    <<Result:160?UINT>> = crypto:hash(sha, Data),
    Result.


%% @spec even(binary()) -> binary().
even(<<X:8,_:8,Z:8>>) ->      <<X:8, Z:8>>;
even(<<X:8,_:8>>) ->          <<X:8>>;
even(<<_:8>>) ->              <<>>;
even(<<X:8,_:8,Z/binary>>) -> <<X:8, (even(Z))/binary>>.

%% @spec odd(binary()) -> binary().
odd(<<_:8,X:8,_:8>>) ->      <<X>>;
odd(<<_:8,X:8>>) ->          <<X>>;
odd(<<X:8>>) ->              <<X>>;
odd(<<_:8,X:8,Z/binary>>) -> <<X:8, (odd(Z))/binary>>.

%% %% @spec binzip(binary(), binary()) -> binary().
%% binzip(<<>>, <<>>) ->
%%     <<>>;
%% binzip(<<H1:8, T1/binary>>, <<H2:8, T2/binary>>) ->
%%     <<(binzip(T1, T2))/binary, H1:8, H2:8>>.
binzip(Bin1, Bin2) ->
    Bin1L = binary_to_list(Bin1),
    Bin2L = binary_to_list(Bin2),
    Zipped = lists:flatten(lists:map(fun(X) -> tuple_to_list(X) end,
                                     lists:zip(Bin1L, Bin2L))),
    list_to_binary(Zipped).

%% @spec challenge(binary()) -> #session{}
challenge(Hash) ->
    Salt       = getBignum(),
    SaltedHash = sha_int(<<Salt:256?UINT, Hash/bytes>>),
    Verifier   = crypto:mod_exp(?g, SaltedHash, ?safe_prime),
    Priv       = getBignum(),
    Gmod       = crypto:mod_exp(?g, Priv, ?safe_prime),
    Pub        = crypto:mod_exp(Verifier * 3 + Gmod, 1, ?safe_prime),
    #session{hash=Hash, salt=Salt, public=Pub, private=Priv, verifier=Verifier}.

%% @spec proof(integer(), integer(), binary()) -> integer().
%% proof(S, ClientPub, _Acc) ->
%%     U  = sha_int(<<ClientPub:256?UINT, (S#session.public):256?UINT>>),
%%     T1 = crypto:compute_key(srp, ClientPub, {S#session.public, S#session.private},
%%                             {host, [binary:encode_unsigned(S#session.verifier),
%%                                     binary:encode_unsigned(?safe_prime),
%%                                     '6', % SRP version
%%                                     binary:encode_unsigned(U)]}),
%%     T2 = crypto:compute_key(srp, ClientPub, {S#session.public, S#session.private},
%%                             {host, [binary:encode_unsigned(S#session.verifier, little),
%%                                     binary:encode_unsigned(?safe_prime, little),
%%                                     '6', % SRP version
%%                                     binary:encode_unsigned(U, little)]}),
%%     SessionKey = sha_int(T1),
%%     SessionKey2 = sha_int(T2),
%%     lager:debug("~nT1 ~p~nT2 ~p~n", [SessionKey, SessionKey2]),
%%     S#session{session_proof = SessionKey2}.

%% proof(S, ClientPub, AccountName) ->
%%     U  = sha_int(<<ClientPub:256?UINT, (S#session.public):256?UINT>>),
%%     S1 = crypto:mod_exp(S#session.verifier, U, ?safe_prime),
%%     S2 = crypto:mod_exp(S1 * ClientPub, S#session.private, ?safe_prime),
%%     T0 = <<S2:256?UINT>>,
%%     T1 = <<(sha_int(even(T0))):160?UINTB>>,
%%     T2 = <<(sha_int(odd(T0))):160?UINTB>>,
%%     SK = binzip(T1, T2),
%%     40 = byte_size(SK), % assert this
%%     SP = sha_int(<<?safe_prime:256?UINT>>),
%%     X  = sha_int(<<?g>>),
%%     SX = SP bxor X,
%%     AN = sha_int(AccountName),
%%     CP = sha_int(<<SX:160?UINT, AN:160?UINT, (S#session.salt):256?UINT,
%%                    ClientPub:256?UINT, (S#session.public):256?UINT, SK/binary>>),
%%     lager:debug("CP ~p~n", [CP]),
%%     SSP = sha_int(<<ClientPub:256?UINT, CP:160?UINT, SK/binary>>),
%%     %SSP = sha_int(<<CP:160?UINT, SK/binary>>),
%%     {ok, S#session{session_key = SK, client_proof = ClientPub,
%%                    session_proof = SSP}}.

%% Implementation according to spec
%% @spec proof(#session{}, integer(), binary()) -> #session{}.
proof(SE, A, I) ->
    fail.

% randomly generated 32 byte number
getBignum() ->
    random:uniform(16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).
