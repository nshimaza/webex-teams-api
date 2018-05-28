# webex-teams-pipes

[![License: MIT](https://img.shields.io/badge/License-MIT-brightgreen.svg)](https://opensource.org/licenses/MIT)
[![Build Status](https://travis-ci.org/nshimaza/webex-teams-api.svg?branch=master)](https://travis-ci.org/nshimaza/webex-teams-api)
[![Hackage](https://img.shields.io/hackage/v/webex-teams-pipes.svg?style=flat)](https://hackage.haskell.org/package/webex-teams-pipes)
[![Stackage Nightly](http://stackage.org/package/webex-teams-pipes/badge/nightly)](http://stackage.org/nightly/package/webex-teams-pipes)
[![Stackage LTS](http://stackage.org/package/webex-teams-pipes/badge/lts)](http://stackage.org/lts/package/webex-teams-pipes)

pipes wrapper of webex-teams-api.

Webex-teams-pipes is thin wrappers of list API.  It transform chunky response
from list API into seamless stream of elements.

A Haskell bindings for Cisco Webex Teams (formerly Cisco Spark) API

This package also provides some sample usage in command line application style.
See source under app directory of the source package.

### Sample Usage

Following example is calling List Membership API which returns membership between
Rooms and users (Person).  You can extract each Membership from
pipes pipe.  The `streamListWithFilter`, provided by webex-teams-pipes,
automatically performs pagenation when it is asked more element and last
response had link of subsequent page in HTTP Link Header.

```haskell
    let auth   = Authorization "your authorization token"
        filter = MembershipFilter yourRoomId Nothing Nothing
    runEffect $ streamListWithFilter auth def filter .| takeC 200 .| mapM_C print (FIXME)
```

You can find more examples in app/Main.hs.

### Limitation

- Relative reference in Link Header is not recognized as next page
