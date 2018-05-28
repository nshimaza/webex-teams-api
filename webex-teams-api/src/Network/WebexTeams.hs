{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Network.CiscoSpark
Copyright   : (c) Naoto Shimazaki 2017
License     : MIT (see the file LICENSE)

Maintainer  : https://github.com/nshimaza
Stability   : experimental

This module provides types and functions for accessing Cisco Spark REST API.

The module is designed to improve type safety over the API.  Each entity is separately typed.
JSON messages contained in REST responses are decoded into appropriate type of Haskell record.
JSON messages sent in REST requests are encoded only from correct type of record.

Some Spark REST API return list of objects.  Those APIs require HTTP Link Header based pagination.
Haskell functions for those APIs automatically request subsequent pages as needed.

= Examples

@
    -- Sending a message to a room.
    let auth        = Authorization "your authorization token"
        roomId      = RoomId "Room ID your message to be sent"
        messageText = MessageText "your message"
        message     = CreateMessage (Just roomId) Nothing Nothing (Just messageText) Nothing Nothing
    createEntity auth def createMessage >>= print . getResponseBody

    -- Obtaining detail of a user.
    let personId    = PersonId "your person ID"
    getDetail auth def personId >>= print . getResponseBody

    -- Obtaining membership of a room as stream of object representing each membership relation.
    let filter = MembershipFilter yourRoomId Nothing Nothing
    runConduit $ streamListWithFilter auth def filter .| takeC 200 .| mapM_C print

    -- Create a room.
    let createRoom  = CreateRoom "Title of the new room" Nothing
    createEntity auth def createRoom >>= print . getResponseBody

    -- Delete a room.
    deleteRoom auth def roomId >>= print . getResponseBody
@

= Support for Lens

This package provides many of records representing objects communicated via Cisco Spark REST API.
Those records are designed to allow create lenses by Control.Lens.TH.makeFields.

Following example creates overloaded accessors for 'Person', 'Room' and 'Team'.

@
makeFields ''Person
makeFields ''Room
makeFields ''Team
@

You can access 'personId', 'roomId' and 'teamId' via overloaded accessor function 'id' like this.

@
    let yourPersonId = yourPerson ^. id
        yourRoomId = yourRoom ^. id
        yourTeamId = yourTeam ^. id
@

This package does not provide pre-generated lenses for you because not everyone need it
but you can make it by yourself so easily as described.


-}
module Network.WebexTeams
    (
    -- * Types
    -- ** Class and Type Families
      SparkFilter
    , SparkListItem
    , ToResponse
    -- ** Common Types
    , Authorization (..)
    , CiscoSparkRequest (..)
    , Timestamp (..)
    , ErrorCode (..)
    , ErrorTitle (..)
    , Errors (..)
    -- ** People related types
    , Person (..)
    , PersonId (..)
    , Email (..)
    , DisplayName (..)
    , NickName (..)
    , FirstName (..)
    , LastName (..)
    , AvatarUrl (..)
    , Timezone (..)
    , PersonStatus (..)
    , PersonType (..)
    , PersonList (..)
    , PersonFilter (..)
    , CreatePerson (..)
    , UpdatePerson (..)
    -- ** Room related types
    , Room (..)
    , RoomId (..)
    , RoomTitle (..)
    , RoomType (..)
    , SipAddr (..)
    , RoomList (..)
    , RoomFilter (..)
    , RoomFilterSortBy (..)
    , CreateRoom (..)
    , UpdateRoom (..)
    -- ** Membership related types
    , Membership (..)
    , MembershipId (..)
    , MembershipList (..)
    , MembershipFilter (..)
    , CreateMembership (..)
    , UpdateMembership (..)
    -- ** Message related types
    , Message (..)
    , MessageId (..)
    , MessageText (..)
    , MessageHtml (..)
    , MessageMarkdown (..)
    , FileUrl (..)
    , MessageList (..)
    , MessageFilter (..)
    , MentionedPeople (..)
    , CreateMessage (..)
    -- ** Team related types
    , TeamName (..)
    , TeamId (..)
    , Team (..)
    , TeamList (..)
    , CreateTeam (..)
    , UpdateTeam (..)
    -- ** Team Membership related types
    , TeamMembership (..)
    , TeamMembershipId (..)
    , TeamMembershipList (..)
    , TeamMembershipFilter (..)
    , CreateTeamMembership (..)
    , UpdateTeamMembership (..)
    -- ** Organization related types
    , Organization (..)
    , OrganizationId (..)
    , OrganizationDisplayName (..)
    , OrganizationList (..)
    -- ** License related types
    , License (..)
    , LicenseId (..)
    , LicenseName (..)
    , LicenseUnit (..)
    , LicenseList (..)
    , LicenseFilter (..)
    -- ** Role related types
    , Role (..)
    , RoleId (..)
    , RoleName (..)
    , RoleList (..)

    -- * Functions
    -- ** Getting detail of an entity
    , getDetail
    , getDetailEither
    -- ** Streaming response of List API with auto pagenation
    , streamEntityWithFilter
    , streamTeamList
    , streamOrganizationList
    , streamRoleList
    , ListReader
    , getListWithFilter
    , getTeamList
    , getOrganizationList
    , getRoleList
    -- ** Creating an entity
    , createEntity
    , createEntityEither
    -- ** Updating an entity
    , updateEntity
    , updateEntityEither
    -- ** Creating default filter spec from mandatory field
    , defaultMessageFilter
    , defaultTeamMembershipFilter
    -- ** Deleting an entity
    , deleteRoom
    , deleteMembership
    , deleteMessage
    , deleteTeam
    , deleteTeamMembership
    ) where

import           Conduit                     (ConduitT, yieldMany)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Data.Aeson                  (FromJSON, ToJSON)
import           Data.ByteString             (ByteString)
import           Data.ByteString.Char8       as C8 (unpack)
import           Data.Default                (Default (def))
import           Data.IORef                  (IORef, newIORef, readIORef,
                                              writeIORef)
import           Data.Maybe                  (catMaybes, maybeToList)
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text)
import           Data.Text.Encoding          (encodeUtf8)
import           Network.HTTP.Simple
import           Network.URI                 (URIAuth (..))

import           Network.WebexTeams.Internal
import           Network.WebexTeams.Types


-- | Authorization string against Spark API to be contained in HTTP Authorization header of every request.
newtype Authorization = Authorization ByteString deriving (Eq, Show)
-- | Wrapping 'Request' in order to provide easy default value specifically for Cisco Spark public API.
data CiscoSparkRequest = CiscoSparkRequest
    { ciscoSparkRequestRequest   :: Request -- ^ Holds pre-set 'Request' for REST API.
    , ciscoSparkRequestScheme    :: String  -- ^ Should be "https:" in production.
    , ciscoSparkRequestAuthority :: URIAuth -- ^ Authority part of request URI.
    } deriving (Show)

-- | Common part of 'Request' against Spark API.
ciscoSparkBaseRequest :: Request
ciscoSparkBaseRequest
    = addRequestHeader "Content-Type" "application/json; charset=utf-8"
    $ setRequestPort 443
    $ setRequestHost "api.ciscospark.com"
    $ setRequestSecure True
    $ defaultRequest

-- | Default parameters for HTTP request to Cisco Spark REST API.
instance Default CiscoSparkRequest where
    def = CiscoSparkRequest ciscoSparkBaseRequest "https:" $ URIAuth "" "api.ciscospark.com" ""

-- | Add given Authorization into request header.
addAuthorizationHeader :: Authorization -> Request -> Request
addAuthorizationHeader (Authorization auth) = addRequestHeader "Authorization" ("Bearer " <> auth)


-- | Building common part of 'Request' for List APIs.
makeCommonListReq
    :: CiscoSparkRequest    -- ^ Common request components
    -> ByteString           -- ^ API category part of REST URL path
    -> CiscoSparkRequest
makeCommonListReq base@CiscoSparkRequest { ciscoSparkRequestRequest = req } path
    = base { ciscoSparkRequestRequest = setRequestPath ("/v1/" <> path) $ setRequestMethod "GET" req }

{-|
    Common worker function for List APIs.
    It accesses List API with given 'Request', unwrap result into list of items, stream them to Conduit pipe
    and finally it automatically accesses next page designated via HTTP Link header if available.
-}
streamList :: (MonadIO m, SparkListItem i) => Authorization -> CiscoSparkRequest -> ConduitT () i m ()
streamList auth (CiscoSparkRequest req scheme uriAuth) = do
    res <- httpJSON $ addAuthorizationHeader auth req
    yieldMany . unwrap $ getResponseBody res
    streamListLoop auth res scheme uriAuth

-- | Processing pagination by HTTP Link header.
streamListLoop :: (MonadIO m, FromJSON a, SparkListItem i) => Authorization -> Response a -> String -> URIAuth -> ConduitT () i m ()
streamListLoop auth res scheme uriAuth
    = case getNextUrl res >>= validateUrl scheme uriAuth >>= (\url -> parseRequest $ "GET " <> C8.unpack url) of
    Nothing         -> pure ()
    Just nextReq    -> do
        nextRes <- httpJSON $ addAuthorizationHeader auth nextReq
        yieldMany . unwrap $ getResponseBody nextRes
        streamListLoop auth nextRes scheme uriAuth

-- | Get list of entities with query parameter and stream it into Conduit pipe.  It automatically performs pagination.
{-# DEPRECATED streamEntityWithFilter "Use getListWithFilter or streamListWithFilter of webex-teams-conduit" #-}
streamEntityWithFilter :: (MonadIO m, SparkFilter filter, SparkListItem (ToResponse filter))
    => Authorization
    -> CiscoSparkRequest
    -> filter
    -> ConduitT () (ToResponse filter) m ()
streamEntityWithFilter auth base param =
    streamList auth $ setQeuryString $ makeCommonListReq base (apiPath param)
      where
        setQeuryString comm@CiscoSparkRequest { ciscoSparkRequestRequest = req }
            = comm { ciscoSparkRequestRequest = setRequestQueryString (toFilterList param) req }

-- | List of 'Team' and stream it into Conduit pipe.  It automatically performs pagination.
{-# DEPRECATED streamTeamList "Use getTeamList or streamTeamList of webex-teams-conduit" #-}
streamTeamList :: MonadIO m => Authorization -> CiscoSparkRequest -> ConduitT () Team m ()
streamTeamList auth base = streamList auth $ makeCommonListReq base teamsPath

-- | Filter list of 'Organization' and stream it into Conduit pipe.  It automatically performs pagination.
{-# DEPRECATED streamOrganizationList "Use getOrganizationList or streamOrganizationList of webex-teams-conduit" #-}
streamOrganizationList :: MonadIO m => Authorization -> CiscoSparkRequest -> ConduitT () Organization m ()
streamOrganizationList auth base = streamList auth $ makeCommonListReq base organizationsPath

-- | List of 'Role' and stream it into Conduit pipe.  It automatically performs pagination.
{-# DEPRECATED streamRoleList "Use getRoleList or streamRoleList of webex-teams-conduit" #-}
streamRoleList :: MonadIO m => Authorization -> CiscoSparkRequest -> ConduitT () Role m ()
streamRoleList auth base = streamList auth $ makeCommonListReq base rolesPath

type ListReader a = IO [a]

{-|
    Returns common worker function 'ListReader' for List APIs.
    ListReader accesses List API with given 'Request' then return responded list of items.
    ListReader also keeps next URL if response is pagenated and next page is available.
    Next call of ListReader causes another List API access for the next page.
    ListReader returns [] when there is no more page.
-}
getList :: (MonadIO m, SparkListItem i) => Authorization -> CiscoSparkRequest -> m (ListReader i)
getList auth wxReq = liftIO $ listReader <$> newIORef (Just wxReq)
  where
    listReader :: SparkListItem i => IORef (Maybe CiscoSparkRequest) -> ListReader i
    listReader wxReqRef = do
        maybeReq <- readIORef wxReqRef
        case maybeReq of
            Nothing                                     -> pure []
            Just (CiscoSparkRequest req scheme uriAuth) -> do
                res <- httpJSON $ addAuthorizationHeader auth req
                writeIORef wxReqRef $ do
                    maybeUrl <- getNextUrl res
                    maybeValidUrl <-validateUrl scheme uriAuth maybeUrl
                    maybeNextReq <- parseRequest $ "GET " <> C8.unpack maybeValidUrl
                    pure (CiscoSparkRequest maybeNextReq scheme uriAuth)
                rr <- readIORef wxReqRef
                pure . unwrap $ getResponseBody res

-- | Get list with query parameter.
getListWithFilter :: (MonadIO m, SparkFilter filter, SparkListItem (ToResponse filter))
    => Authorization
    -> CiscoSparkRequest
    -> filter
    -> m (ListReader (ToResponse filter))
getListWithFilter auth base param =
    getList auth $ setQeuryString $ makeCommonListReq base (apiPath param)
      where
        setQeuryString comm@CiscoSparkRequest { ciscoSparkRequestRequest = req }
            = comm { ciscoSparkRequestRequest = setRequestQueryString (toFilterList param) req }

-- | Return 'ListReader' for 'Team'.
getTeamList :: MonadIO m => Authorization -> CiscoSparkRequest -> m (ListReader Team)
getTeamList auth base = getList auth $ makeCommonListReq base teamsPath

-- | Return 'ListReader' for 'Team'.
getOrganizationList :: MonadIO m => Authorization -> CiscoSparkRequest -> m (ListReader Organization)
getOrganizationList auth base = getList auth $ makeCommonListReq base organizationsPath

-- | Return 'ListReader' for 'Team'.
getRoleList :: MonadIO m => Authorization -> CiscoSparkRequest -> m (ListReader Role)
getRoleList auth base = getList auth $ makeCommonListReq base rolesPath

makeCommonDetailReq
    :: CiscoSparkRequest    -- ^ Common request components.
    -> Authorization        -- ^ Authorization string against Spark API.
    -> ByteString           -- ^ API category part of REST URL path.
    -> Text                 -- ^ Identifier string part of REST URL path.
    -> Request
makeCommonDetailReq (CiscoSparkRequest base _ _) auth path idStr
    = setRequestPath ("/v1/" <> path <> "/" <> encodeUtf8 idStr)
    $ setRequestMethod "GET"
    $ addAuthorizationHeader auth
    $ base

{-|
    Get details of a Spark entity.

    Obtaining detail of an entity identified by key.  The key can be a value in one of
    following types: 'PersonId', 'RoomId', 'MembershipId', 'MessageId', 'TeamId', 'TeamMembershipId',
    'OrganizationId', 'LicenseId', 'RoleId'.  API is automatically selected by type of the key.
    A JSONException runtime exception will be thrown on an JSON parse errors.
-}
getDetail :: (MonadIO m, SparkDetail key)
    => Authorization        -- ^ Authorization string against Spark API.
    -> CiscoSparkRequest    -- ^ Predefined part of 'Request' commonly used for Cisco Spark API.
    -> key                  -- ^ One of PersonId, RoomId, MembershipId, MessageId, TeamId, TeamMembershipId,
                            --   OrganizationId, LicenseId and RoleId.
    -> m (Response (ToResponse key))
getDetail auth base entityId = httpJSON $ makeCommonDetailReq base auth (apiPath entityId) (toIdStr entityId)

-- | Get details of a Spark entity.  A Left value will be returned on an JSON parse errors.
getDetailEither :: (MonadIO m, SparkDetail key)
    => Authorization
    -> CiscoSparkRequest
    -> key
    -> m (Response (Either JSONException (ToResponse key)))
getDetailEither auth base entityId = httpJSONEither $ makeCommonDetailReq base auth (apiPath entityId) (toIdStr entityId)


makeCommonCreateReq :: ToJSON a => CiscoSparkRequest -> Authorization -> ByteString -> a -> Request
makeCommonCreateReq (CiscoSparkRequest base _ _) auth path body
    = setRequestBodyJSON body
    $ setRequestPath ("/v1/" <> path)
    $ setRequestMethod "POST"
    $ addAuthorizationHeader auth
    $ base

{-|
    Create a Spark entity with given parameters.

    Creating a new entity of Spark such as space, team, membership or message.
    REST API path is automatically selected by type of createParams.
    A JSONException runtime exception will be thrown on an JSON parse errors.
-}
createEntity :: (MonadIO m, SparkCreate createParams)
    => Authorization        -- ^ Authorization string against Spark API.
    -> CiscoSparkRequest    -- ^ Predefined part of 'Request' commonly used for Cisco Spark API.
    -> createParams         -- ^ One of 'CreatePerson', 'CreateRoom', 'CreateMembership', 'CreateMessage',
                            --   'CreateTeam' and 'CreateTeamMembership'.
    -> m (Response (ToResponse createParams))
createEntity auth base param = httpJSON $ makeCommonCreateReq base auth (apiPath param) param

-- | Create a Spark entity with given parameters.  A Left value will be returned on an JSON parse errors.
createEntityEither :: (MonadIO m, SparkCreate createParams)
    => Authorization
    -> CiscoSparkRequest
    -> createParams
    -> m (Response (Either JSONException (ToResponse createParams)))
createEntityEither auth base param = httpJSONEither $ makeCommonCreateReq base auth (apiPath param) param


makeCommonUpdateReq :: ToJSON a => CiscoSparkRequest -> Authorization -> ByteString -> a -> Request
makeCommonUpdateReq (CiscoSparkRequest base _ _) auth path body
    = setRequestBodyJSON body
    $ setRequestPath ("/v1/" <> path)
    $ setRequestMethod "PUT"
    $ addAuthorizationHeader auth
    $ base

{-|
    Update a Spark entity with given parameters.

    Creating a new entity of Spark such as space, team, or membership.
    REST API path is automatically selected by type of updateParams.
    A JSONException runtime exception will be thrown on an JSON parse errors.
-}
updateEntity :: (MonadIO m, SparkUpdate updateParams)
    => Authorization        -- ^ Authorization string against Spark API.
    -> CiscoSparkRequest    -- ^ Predefined part of 'Request' commonly used for Cisco Spark API.
    -> updateParams         -- ^ One of 'UpdatePerson', 'UpdateRoom', 'UpdateMembership',
                            --   'UpdateTeam' and 'UpdateTeamMembership'.
    -> m (Response (ToResponse updateParams))
updateEntity auth base param = httpJSON $ makeCommonUpdateReq base auth (apiPath param) param

-- | Update a Spark entity with given parameters.  A Left value will be returned on an JSON parse errors.
updateEntityEither :: (MonadIO m, SparkUpdate updateParams)
    => Authorization
    -> CiscoSparkRequest
    -> updateParams
    -> m (Response (Either JSONException (ToResponse updateParams)))
updateEntityEither auth base param = httpJSONEither $ makeCommonUpdateReq base auth (apiPath param) param


makeCommonDeleteReq
    :: Authorization    -- ^ Authorization string against Spark API.
    -> Request          -- ^ Common request components.
    -> ByteString       -- ^ API category part of REST URL path.
    -> Text             -- ^ Identifier string part of REST URL path.
    -> Request
makeCommonDeleteReq auth base path idStr
    = setRequestPath ("/v1/" <> path <> "/" <> encodeUtf8 idStr)
    $ setRequestMethod "DELETE"
    $ addAuthorizationHeader auth
    $ base

-- | Polymorphic version of delete.  Intentionally not exposed to outside of the module.
deleteEntity :: (MonadIO m, SparkDetail key)
    => Authorization        -- ^ Authorization string against Spark API.
    -> CiscoSparkRequest    -- ^ Predefined part of 'Request' commonly used for Cisco Spark API.
    -> key                  -- ^ One of PersonId, RoomId, MembershipId, MessageId, TeamId, TeamMembershipId.
    -> m (Response ())
deleteEntity auth (CiscoSparkRequest base _ _) entityId
    = httpNoBody $ makeCommonDeleteReq auth base (apiPath entityId) (toIdStr entityId)

-- | Deletes a room, by ID.
deleteRoom :: MonadIO m
    => Authorization        -- ^ Authorization string against Spark API.
    -> CiscoSparkRequest    -- ^ Predefined part of 'Request' commonly used for Cisco Spark API.
    -> RoomId               -- ^ Identifier of a space to be deleted.
    -> m (Response ())
deleteRoom = deleteEntity

-- | Deletes a membership, by ID.
deleteMembership :: MonadIO m
    => Authorization        -- ^ Authorization string against Spark API.
    -> CiscoSparkRequest    -- ^ Predefined part of 'Request' commonly used for Cisco Spark API.
    -> MembershipId         -- ^ Identifier of a space to be deleted.
    -> m (Response ())
deleteMembership = deleteEntity

-- | Deletes a message, by ID.
deleteMessage :: MonadIO m
    => Authorization        -- ^ Authorization string against Spark API.
    -> CiscoSparkRequest    -- ^ Predefined part of 'Request' commonly used for Cisco Spark API.
    -> MessageId            -- ^ Identifier of a space to be deleted.
    -> m (Response ())
deleteMessage = deleteEntity

-- | Deletes a team, by ID.
deleteTeam :: MonadIO m
    => Authorization        -- ^ Authorization string against Spark API.
    -> CiscoSparkRequest    -- ^ Predefined part of 'Request' commonly used for Cisco Spark API.
    -> TeamId               -- ^ Identifier of a space to be deleted.
    -> m (Response ())
deleteTeam = deleteEntity

-- | Deletes a teamMembership, by ID.
deleteTeamMembership :: MonadIO m
    => Authorization        -- ^ Authorization string against Spark API.
    -> CiscoSparkRequest    -- ^ Predefined part of 'Request' commonly used for Cisco Spark API.
    -> TeamMembershipId     -- ^ Identifier of a space to be deleted.
    -> m (Response ())
deleteTeamMembership = deleteEntity
