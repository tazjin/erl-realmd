-module(srp6).
-export([challenge/1, proof/3]).

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

%% @spec binzip(binary(), binary()) -> binary().
binzip(<<>>, <<>>) ->
    <<>>;
binzip(<<H1:8, T1/binary>>, <<H2:8, T2/binary>>) ->
    <<(binzip(T1, T2))/binary, H1:8, H2:8>>.

%% @spec challenge(binary()) -> #session{}
challenge(Hash) ->
    Salt       = getBignum(),
    SaltedHash = sha_int(<<Salt:256?UINT, Hash/bytes>>),
    Verifier   = crypto:mod_exp(?g, SaltedHash, 15),
    Priv       = getBignum(),
    Gmod       = crypto:mod_exp(?g, Priv, ?safe_prime),
    Pub        = crypto:mod_exp(Verifier * 3 + Gmod, 1, ?safe_prime),
    #session{hash=Hash, salt=Salt, public=Pub, private=Priv, verifier=Verifier}.

proof(S, ClientPub, AccountName) ->
    U  = sha_int(<<ClientPub:256?UINT, (S#session.public):256?UINT>>),
    S1 = crypto:mod_exp(S#session.verifier, U, ?safe_prime),
    S2 = crypto:mod_exp(S1 * ClientPub, S#session.private, ?safe_prime),
    T0 = <<S2:256?UINT>>,
    T1 = <<(sha_int(even(T0))):160?UINTB>>,
    T2 = <<(sha_int(odd(T0))):160?UINTB>>,
    SK = binzip(T1, T2),
    SP = sha_int(<<?safe_prime:256?UINT>>),
    X  = sha_int(<<?g>>),
    SX = SP bxor X,
    AN = sha_int(AccountName),
    CP = sha_int(<<SX:160?UINT, AN:160?UINT, (S#session.salt):256?UINT,
                   ClientPub:256?UINT, (S#session.public):256?UINT, SK/binary>>),
    SSP = sha_int(<<ClientPub:256?UINT, CP:160?UINT, SK/binary>>),
    S#session{session_key = SK, client_proof = CP, session_proof = SSP}.

% randomly generated 32 byte number
getBignum() ->
    random:uniform(16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).
