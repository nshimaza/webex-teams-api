{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.CiscoSpark
    (
    -- * Types
      Authorization (..)
    , CiscoSparkRequest (..)
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

    , Timestamp (..)
    -- * Functions
    , getDetail
    , getDetailEither
    , streamEntityWithFilter
    , createEntity
    , createEntityEither
    , updateEntity
    , updateEntityEither
    , defaultMessageFilter
    , streamTeamList
    , streamOrganizationList
    , streamRoleList
    ) where

import           Conduit
import           Data.Aeson                  (FromJSON, ToJSON)
import           Data.ByteString             (ByteString)
import           Data.ByteString.Char8       as C8 (unpack)
import           Data.Default                (Default (def))
import           Data.Maybe                  (maybeToList, catMaybes)
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text)
import           Data.Text.Encoding          (encodeUtf8)
import           Network.HTTP.Simple

import           Network.CiscoSpark.Internal
import           Network.CiscoSpark.Types








-- | Authorization string against Spark API to be contained in HTTP Authorization header of every request.
newtype Authorization = Authorization ByteString deriving (Eq, Show)
-- | Wrapping 'Request' in order to provide easy default value specifically for Cisco Spark public API.
newtype CiscoSparkRequest = CiscoSparkRequest Request deriving (Show)

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
    def = CiscoSparkRequest ciscoSparkBaseRequest

-- | Add given Authorization into request header.
addAuthorizationHeader :: Authorization -> Request -> Request
addAuthorizationHeader (Authorization auth) = addRequestHeader "Authorization" ("Bearer " <> auth)


-- | Building common part of 'Request' for List APIs.
makeCommonListReq
    :: CiscoSparkRequest    -- ^ Common request components
    -> ByteString           -- ^ API category part of REST URL path
    -> Request
makeCommonListReq (CiscoSparkRequest base) path = setRequestPath ("/v1/" <> path)
                                                $ setRequestMethod "GET"
                                                $ base

{-|
    Common worker function for List APIs.
    It accesses List API with given 'Request', unwrap result into list of items, stream them to Conduit pipe
    and finally it automatically accesses next page designated via HTTP Link header if available.
-}
streamList :: (MonadIO m, SparkListItem i) => Authorization -> Request -> Source m i
streamList auth req = do
    res <- httpJSON $ addAuthorizationHeader auth req
    yieldMany . unwrap $ getResponseBody res
    streamListLoop auth res

-- | Processing pagination by HTTP Link header.
streamListLoop :: (MonadIO m, FromJSON a, SparkListItem i) => Authorization -> Response a -> Source m i
streamListLoop auth res = case getNextUrl res >>= (\url -> parseRequest $ "GET " <> (C8.unpack url)) of
    Nothing         -> pure ()
    Just nextReq    -> do
        nextRes <- httpJSON $ addAuthorizationHeader auth nextReq
        yieldMany . unwrap $ getResponseBody nextRes
        streamListLoop auth nextRes

-- | Get list of entities with query parameter and stream it into Conduit pipe.  It automatically performs pagination.
streamEntityWithFilter :: (MonadIO m, SparkFilter filter, SparkListItem (ToResponse filter))
    => Authorization
    -> CiscoSparkRequest
    -> filter
    -> Source m (ToResponse filter)
streamEntityWithFilter auth base param =
    streamList auth $ setRequestQueryString (toFilterList param) $ makeCommonListReq base (apiPath param)

-- | List of 'Team' and stream it into Conduit pipe.  It automatically performs pagination.
streamTeamList :: MonadIO m => Authorization -> CiscoSparkRequest -> Source m Team
streamTeamList auth base = streamList auth $ makeCommonListReq base teamsPath

-- | Filter list of 'Organization' and stream it into Conduit pipe.  It automatically performs pagination.
streamOrganizationList :: MonadIO m => Authorization -> CiscoSparkRequest -> Source m Organization
streamOrganizationList auth base = streamList auth $ makeCommonListReq base organizationsPath

-- | List of 'Role' and stream it into Conduit pipe.  It automatically performs pagination.
streamRoleList :: MonadIO m => Authorization -> CiscoSparkRequest -> Source m Role
streamRoleList auth base = streamList auth $ makeCommonListReq base rolesPath


makeCommonDetailReq
    :: CiscoSparkRequest    -- ^ Common request components.
    -> Authorization        -- ^ Authorization string against Spark API.
    -> ByteString           -- ^ API category part of REST URL path.
    -> Text                 -- ^ Identifier string part of REST URL path.
    -> Request
makeCommonDetailReq (CiscoSparkRequest base) auth path idStr
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
makeCommonCreateReq (CiscoSparkRequest base) auth path body
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
    -> CiscoSparkRequest    -- ^ Predefined part of 'Request' commonly used for Cisco Spark API
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
makeCommonUpdateReq (CiscoSparkRequest base) auth path body
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
    -> CiscoSparkRequest    -- ^ Predefined part of 'Request' commonly used for Cisco Spark API
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
