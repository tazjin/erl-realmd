WoW realm list
==============

The WoW realmlist is sent from the login server to the WoW client and contains
the list of all realms, their IP addresses, ports and relevant metadata.

MaNGOS stores a realm like this:

```c++
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

enum AccountTypes
{
    SEC_PLAYER         = 0,
    SEC_MODERATOR      = 1,
    SEC_GAMEMASTER     = 2,
    SEC_ADMINISTRATOR  = 3,
    SEC_CONSOLE        = 4                                  // must be always last in list, accounts must have less security level always also
};

typedef std::set<uint32> RealmBuilds;

struct RealmBuildInfo
{
    int build; /**< TODO */
    int major_version; /**< TODO */
    int minor_version; /**< TODO */
    int bugfix_version; /**< TODO */
    int hotfix_version; /**< TODO */
};

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

Which would translate into roughly

```haskell
import Data.Word

-- Without extra types

data Realm = Realm {
      _name :: String
    , _address :: Text
    , _icon :: Word8
    , _realmFlags :: RealmFlags
    , _timezone :: Word8
    , _mId :: Word32 -- Don't know what this is
    , _allowedSecurityLevel :: AccountTypes
    , _populationLevel :: Float
    , _realmBuilds :: RealmBuilds
    , _realmBuildInfo :: RealmBuildInfo
}
```

