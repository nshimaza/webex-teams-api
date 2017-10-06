# cisco-spark-api
A Haskell bindings for Cisco Spark API

Cisco-spark-api package provides types and functions for accessing Cisco Spark REST API.

This package is designed to improve type safety over the API.  Each entity is separately typed.
JSON messages contained in REST responses are decoded into appropriate type of Haskell record.
JSON messages sent in REST requests are encoded only from correct type of record.

Some Spark REST API returning list of objects require HTTP Link Header based pagination.
Haskell functions abstract it.  They automatically request subsequent pages as needed and
seamlessly stream returned elements rather than just return a chunk of elements in a response.

This package also provides some sample usage in command line application style.
See source under app directory of the source package.

### Sample Usage

Following example is calling List Membership API which returns membership between
Spark spaces (Room) and Spark users (Person).  You can extract each Membership from
Conduit pipe.  The streamEntityWithFilter automatically perform pagenation when it is
asked more element and last response indicated subsequent page in HTTP Link Header.

```Haskell
    let auth   = Authorization "your authorization token"
        filter = MembershipFilter yourRoomId Nothing Nothing
    runConduit $ streamEntityWithFilter auth def filter .| takeC 200 .| mapM_C print
```

### Support for Lens

This package provides many of records representing objects communicated via Cisco Spark REST API.
Those records are designed to allow create lenses by Control.Lens.TH.makeFields.

Following example creates overloaded accessors for 'Person', 'Room' and 'Team'.

```Haskell
makeFields ''Person
makeFields ''Room
makeFields ''Team
```

You can access 'personId', 'roomId' and 'teamId' via overloaded accessor function 'id' like this.

```Haskell
    let yourPersonId = yourPerson ^. id
        yourRoomId = yourRoom ^. id
        yourTeamId = yourTeam ^. id
```

This package does not provide pre-generated lenses for you because not everyone need it
but you can make it by yourself so easily as described.

### Limitation

Following items are not yet implemented.

- WebHook
- Element level error in List API

### Known Security Hole

Current implementation does not check Link Header URL points same host and schema with responding server.
