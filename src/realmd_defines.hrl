%% Login error codes
-define(ce_success, 0).
-define(ce_ipban, 1).
-define(ce_account_closed, 3).
-define(ce_no_account, 4).
-define(ce_account_in_use, 6).
-define(ce_preorder_time_limit, 7).
-define(ce_server_full, 8).
-define(ce_wrong_build_number, 9).
-define(ce_update_client, 10).
-define(ce_account_freezed, 12).

%% Network defines
-define(UINT, /unsigned-little-integer).
-define(UINTB, /unsigned-big-integer).

%% SRP 6 defines
-define(safe_prime, 16#894B645E89E1535BBDAD5B8B290650530801B18EBFBF5E8FAB3C82872A3E9BB7).
-define(g, 7).
-define(k, 3).

%% Login records (to keep argument passing sane)
-record(session, { public
                 , private
                 , verifier
                 , salt
                 , hash
                 , session_key
                 , session_proof
                 , client_proof}).
