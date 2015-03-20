WoW 1.12 login server in Haskell
================================

## General procedure in C++ version

I'm leaving out all unnecessary details (config loading etc.). The code looks pretty
messy.


### Realmlist
There's a RealmList singleton (sRealmList) that gets initialized with the update
delay (20 seconds by default). It then begins calling UpdateRealms.

**UpdateRealms**:

`void RealmList::UpdateRealms(bool init)`

Fetches all realms:

`QueryResult* result = LoginDatabase.Query("SELECT id, name, address, port, icon, realmflags, timezone, allowedSecurityLevel, population, realmbuilds FROM realmlist WHERE (realmflags & 1) = 0 ORDER BY name");`

Realms are stored as this struct:

```c++
struct Realm
{
    std::string name;
    std::string address;
    uint8 icon;
    RealmFlags realmflags;                                  // realmflags
    uint8 timezone;
    uint32 m_ID;
    AccountTypes allowedSecurityLevel;                      // current allowed join security level (show as locked for not fit accounts)
    float populationLevel;
    RealmBuilds realmbuilds;                                // list of supported builds (updated in DB by mangosd)
    RealmBuildInfo realmBuildInfo;                          // build info for show version in list
};
```

The `RealmList` has a `typedef std::map<std::string, Realm> RealmMap;`.

**Expired bans**:

A DB transaction is run that removes expired bans. This is presumably also run at intervals,
but not relevant for getting this to work.

**Listening socket**:

Using [ACE](http://www.cs.wustl.edu/~schmidt/ACE.html), `realmd` creates the listening socket.
I can ignore all the config for now and just go with it binding on `0.0.0.0:$DEFAULT_PORT` by
default.

The socket is created from `AuthSocket`.

**Random stuff 1**:
realmd waits for the socket to come up, registers signal handlers, sets process priority stuff etc.

The main loop has some manual database pinging to keep the connection alive (??).


**Constant stuff**:

There's a bunch of relevant enums in `shared/Common.h`

```c++
enum AccountTypes
{
    SEC_PLAYER         = 0,
    SEC_MODERATOR      = 1,
    SEC_GAMEMASTER     = 2,
    SEC_ADMINISTRATOR  = 3,
    SEC_CONSOLE        = 4                                  // must be always last in list, accounts must have less security level always also
};

enum RealmFlags
{
    REALM_FLAG_NONE         = 0x00,
    REALM_FLAG_INVALID      = 0x01,
    REALM_FLAG_OFFLINE      = 0x02,
    REALM_FLAG_SPECIFYBUILD = 0x04,                         // client will show realm version in RealmList screen in form "RealmName (major.minor.revision.build)"
    REALM_FLAG_UNK1         = 0x08,
    REALM_FLAG_UNK2         = 0x10,
    REALM_FLAG_NEW_PLAYERS  = 0x20,
    REALM_FLAG_RECOMMENDED  = 0x40,
    REALM_FLAG_FULL         = 0x80
};
```

