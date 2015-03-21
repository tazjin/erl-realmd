-module(realmd_user).
-export([get_srp_ingredients/1]).

%% This module provides functions for retrieving user info from the database.

%% Mock module atm (we don't have a DB!)

%% Mock values
-define(MOCK_SALT, 15507576536481420705407266319657717926220654946187700281894158528917108380951).

get_srp_ingredients(<<"TEST">>) ->
    {ok, {salt, binary:encode_unsigned(?MOCK_SALT)},
     {hash, <<"3d0d99423e31fcc67a6745ec89d70d700344bc76">>}};
get_srp_ingredients(<<"BANNED">>) ->
    {error, banned};
get_srp_ingredients(Username) ->
    {error, notfound}.


%% get_salt(<<"TEST">>) ->
%%     {ok, binary:encode_unsigned(?MOCK_SALT)};
%% get_salt(Username) -> {error, notfound}.

%% get_hash(<<"TEST">>) ->
%%     {ok, <<"3d0d99423e31fcc67a6745ec89d70d700344bc76">>};
%% get_hash(Username) -> {error, notfound}.
