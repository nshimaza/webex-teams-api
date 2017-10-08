# cisco-spark-api

[![License: MIT](https://img.shields.io/badge/License-MIT-brightgreen.svg)](https://opensource.org/licenses/MIT)
[![Build Status](https://travis-ci.org/nshimaza/cisco-spark-api.svg?branch=master)](https://travis-ci.org/nshimaza/cisco-spark-api)
[![Hackage](https://img.shields.io/hackage/v/cisco-spark-api.svg?style=flat)](https://hackage.haskell.org/package/cisco-spark-api)
[![Stackage Nightly](http://stackage.org/package/cisco-spark-api/badge/nightly)](http://stackage.org/nightly/package/cisco-spark-api)
[![Stackage LTS](http://stackage.org/package/cisco-spark-api/badge/lts)](http://stackage.org/lts/package/cisco-spark-api)

A Haskell bindings for Cisco Spark API

Cisco-spark-api package provides types and functions for accessing Cisco Spark REST API.
The detail of Spark API is available from [developer site of Cisco Spark](https://developer.ciscospark.com/)

This package is designed to improve type safety over the API.  Each entity is separately typed.
JSON messages contained in REST responses are decoded into appropriate type of Haskell record.
JSON messages sent in REST requests are encoded only from correct type of record.

Some Spark REST API return list of objects.  Those APIs require HTTP Link Header based pagination.
Haskell functions for those APIs automatically request subsequent pages as needed.
Also those functions transform chunky response into seamless stream of elements.

This package also provides some sample usage in command line application style.
See source under app directory of the source package.

### Sample Usage

Sending a message to a Spark Space (Room).

```haskell
    let auth        = Authorization "your authorization token"
        roomId      = RoomId "Room ID your message to be sent"
        messageText = MessageText "your message"
        message     = CreateMessage (Just roomId) Nothing Nothing (Just messageText) Nothing Nothing
    createEntity auth def createMessage >>= print . getResponseBody
```

Following example is calling List Membership API which returns membership between
Spark spaces (Room) and Spark users (Person).  You can extract each Membership from
Conduit pipe.  The streamEntityWithFilter automatically performs pagenation when it is
asked more element and last response indicated subsequent page in HTTP Link Header.

```haskell
    let auth   = Authorization "your authorization token"
        filter = MembershipFilter yourRoomId Nothing Nothing
    runConduit $ streamEntityWithFilter auth def filter .| takeC 200 .| mapM_C print
```

You can find more examples in app/Main.hs

### Support for Lens

This package provides many of records representing objects communicated via Cisco Spark REST API.
Those records are designed to allow create lenses by Control.Lens.TH.makeFields.

Following example creates overloaded accessors for 'Person', 'Room' and 'Team'.

```haskell
makeFields ''Person
makeFields ''Room
makeFields ''Team
```

You can access 'personId', 'roomId' and 'teamId' via overloaded accessor function 'id' like this.

```haskell
    let yourPersonId = yourPerson ^. id
        yourRoomId = yourRoom ^. id
        yourTeamId = yourTeam ^. id
```

This package does not provide pre-generated lenses for you because not everyone need it
but you can make it by yourself so easily as described.

### Limitation

- WebHook API is not implemented.
- Relative reference in Link Header is not recognized as next page
