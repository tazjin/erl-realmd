WoW realmd crypto stuff
=======================

## Password transmission

WoW uses [SRP6](http://srp.stanford.edu/design.html) for password transmissions.
MaNGOS implements it like so (snippets from AuthSocket.<cpp/h>):

```c++
    private:

        BigNumber N, s, g, v; /**< TODO */
        BigNumber b, B; /**< TODO */
        BigNumber K; /**< TODO */
        BigNumber _reconnectProof; /**< TODO */


AuthSocket::AuthSocket()
{
    N.SetHexStr("894B645E89E1535BBDAD5B8B290650530801B18EBFBF5E8FAB3C82872A3E9BB7");
    g.SetDword(7);
    _authed = false;

    _accountSecurityLevel = SEC_PLAYER;

    _build = 0;
    patch_ = ACE_INVALID_HANDLE;
}

```
and stuff in shared/Auth/BigNumber
