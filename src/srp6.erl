-module(srp6).

-compile(export_all).

%% SRP6 constants
-define(safe_prime, <<16#894B645E89E1535BBDAD5B8B290650530801B18EBFBF5E8FAB3C82872A3E9BB7:256>>).
-define(srp_version, '6').

-define(n_length, 32).
-define(g, <<7>>).
-define(k, <<3>>).

%% Calculate verifier (v = g^x%N, x=sha1(salt, hash))
calcVerifier(Hash, Salt) ->
    SaltedHash = crypto:hash(sha, [Salt, Hash]),
    crypto:mod_pow(?g, SaltedHash, ?safe_prime).

calcPublic(Verifier) ->
    PrivateKey = getPrivate(),
    {Pub, PrivateKey} =
        crypto:generate_key(srp,{host,[Verifier, ?g, ?safe_prime, ?srp_version]},
                            PrivateKey),
    Pub.

% randomly generated 32 byte number
getPrivate() ->
    crypto:strong_rand_bytes(32).

getUsername() -> <<"TEST">>.
getPassword() -> <<"TEST">>.
getHash() -> <<"3d0d99423e31fcc67a6745ec89d70d700344bc76">>.
getSalt() -> crypto:strong_rand_bytes(32). %<<"STROGNSALT">>.
getPrime() -> ?safe_prime.

srp6_calc_B(Salt, I, P) -> ok.
