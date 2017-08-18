{-# LANGUAGE DataKinds         #-}
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
    , PersonQuery (..)
    , CreatePerson (..)
    , UpdatePerson (..)
    -- ** Room related types
    , Room (..)
    , RoomId (..)
    , RoomTitle (..)
    , RoomType (..)
    , SipAddr (..)
    , RoomList (..)
    , RoomQuery (..)
    , RoomQuerySortBy (..)
    , CreateRoom (..)
    , UpdateRoom (..)
    -- ** Membership related types
    , Membership (..)
    , MembershipId (..)
    , MembershipList (..)
    , MembershipQuery (..)
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
    , MessageQuery (..)
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
    , TeamMembershipQuery (..)
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
    , LicenseQuery (..)
    -- ** Role related types
    , Role (..)
    , RoleId (..)
    , RoleName (..)
    , RoleList (..)

    , Timestamp (..)
    -- * Functions
    , getDetail
    , getDetailEither
    , createEntity
    , createEntityEither
    -- ** People
    , streamPersonList
    -- ** Rooms
    , streamRoomList
    -- ** Memberships
    , streamMembershipList
    -- ** Messages
    , defaultMessageQuery
    , streamMessageList
    -- ** Teams
    , streamTeamList
    -- ** Team Memberships
    , streamTeamMembershipList
    -- ** Organizations
    , streamOrganizationList
    -- ** Licenses
    , streamLicenseList
    -- ** Roles
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

-- | Query list of 'Person' and stream it into Conduit pipe.  It automatically performs pagination.
streamPersonList :: MonadIO m => Authorization -> CiscoSparkRequest -> PersonQuery -> Source m Person
streamPersonList auth base query = streamList auth $ setRequestQueryString qs $ makeCommonListReq base peoplePath
  where
    qs = catMaybes [ (\(Email e) -> ("email", Just $ encodeUtf8 e)) <$> personQueryEmail query
                   , (\(DisplayName n) -> ("displayName", Just (encodeUtf8 n))) <$> personQueryDisplayName query
                   , (\(OrganizationId o) -> ("orgId", Just (encodeUtf8 o))) <$> personQueryOrgId query
                   ]

-- | Query list of 'Room' and stream it into Conduit pipe.  It automatically performs pagination.
streamRoomList :: MonadIO m => Authorization -> CiscoSparkRequest -> RoomQuery -> Source m Room
streamRoomList auth base query = streamList auth $ setRequestQueryString qs $ makeCommonListReq base roomsPath
  where
    qs = catMaybes [ (\(TeamId e) -> ("teamId", Just $ encodeUtf8 e)) <$> roomQueryTeamId query
                   , (\t -> ("type", Just $ roomTypeToQueryString t)) <$> roomQueryRoomType query
                   , (\o -> ("sortBy", Just $ roomQuerySortByToQueryString o)) <$> roomQuerySortBy query
                   ]

-- | Query list of 'Membership' and stream it into Conduit pipe.  It automatically performs pagination.
streamMembershipList :: MonadIO m => Authorization -> CiscoSparkRequest -> MembershipQuery -> Source m Membership
streamMembershipList auth base query = streamList auth $ setRequestQueryString qs $ makeCommonListReq base membershipsPath
  where
    qs = catMaybes [ (\(RoomId r) -> ("roomId", Just $ encodeUtf8 r)) <$> membershipQueryRoomId query
                   , (\(PersonId p) -> ("personId", Just $ encodeUtf8 p)) <$> membershipQueryPersonId query
                   , (\(Email e) -> ("personEmail", Just $ encodeUtf8 e)) <$> membershipQueryPersonEmail query
                   ]

-- | Query list of 'Message' and stream it into Conduit pipe.  It automatically performs pagination.
streamMessageList :: MonadIO m => Authorization -> CiscoSparkRequest -> MessageQuery -> Source m Message
streamMessageList auth base query = streamList auth $ setRequestQueryString qs $ makeCommonListReq base messagesPath
  where
    qs = ("roomId", Just $ encodeUtf8 rid) : catMaybes
        [ (\p -> ("mentionedPeople", Just $ mentionedPeopleToQueryString p)) <$> messageQueryMentionedPeople query
        , (\(Timestamp t) -> ("before", Just $ encodeUtf8 t)) <$> messageQueryBefore query
        , (\(MessageId m) -> ("beforeMessage", Just $ encodeUtf8 m)) <$> messageQueryBeforeMessage query
        ]
    (RoomId rid) = messageQueryRoomId query

-- | Query list of 'Team' and stream it into Conduit pipe.  It automatically performs pagination.
streamTeamList :: MonadIO m => Authorization -> CiscoSparkRequest -> Source m Team
streamTeamList auth base = streamList auth $ makeCommonListReq base teamsPath

-- | Query list of 'TeamMembership' and stream it into Conduit pipe.  It automatically performs pagination.
streamTeamMembershipList :: MonadIO m => Authorization -> CiscoSparkRequest -> TeamMembershipQuery -> Source m TeamMembership
streamTeamMembershipList auth base query = streamList auth $ setRequestQueryString qs $ makeCommonListReq base teamMembershipsPath
  where
    qs = maybeToList $ (\(TeamId t) -> ("teamId", Just $ encodeUtf8 t)) <$> teamMembershipQueryTeamId query

-- | Query list of 'Organization' and stream it into Conduit pipe.  It automatically performs pagination.
streamOrganizationList :: MonadIO m => Authorization -> CiscoSparkRequest -> Source m Organization
streamOrganizationList auth base = streamList auth $ makeCommonListReq base organizationsPath

-- | Query list of 'License' and stream it into Conduit pipe.  It automatically performs pagination.
streamLicenseList :: MonadIO m => Authorization -> CiscoSparkRequest -> LicenseQuery -> Source m License
streamLicenseList auth base query = streamList auth $ setRequestQueryString qs $ makeCommonListReq base licensesPath
  where
    qs = maybeToList $ (\(OrganizationId o) -> ("orgId", Just $ encodeUtf8 o)) <$> licenseQueryOrgId query

-- | Query list of 'Role' and stream it into Conduit pipe.  It automatically performs pagination.
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
    => CiscoSparkRequest    -- ^ Predefined part of 'Request' commonly used for Cisco Spark API.
    -> Authorization        -- ^ Authorization string against Spark API.
    -> key                  -- ^ One of PersonId, RoomId, MembershipId, MessageId, TeamId, TeamMembershipId,
                            --   OrganizationId, LicenseId and RoleId.
    -> m (Response (ToDetailResponse key))
getDetail base auth entityId = httpJSON $ makeCommonDetailReq base auth (detailPath entityId) (toIdStr entityId)

-- | Get details of a Spark entity.  A Left value will be returned on an JSON parse errors.
getDetailEither :: (MonadIO m, SparkDetail key)
    => CiscoSparkRequest
    -> Authorization
    -> key
    -> m (Response (Either JSONException (ToDetailResponse key)))
getDetailEither base auth entityId = httpJSONEither $ makeCommonDetailReq base auth (detailPath entityId) (toIdStr entityId)


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
createEntity :: (MonadIO m, SparkCreate createParams, ToJSON createParams)
    => CiscoSparkRequest    -- ^ Predefined part of 'Request' commonly used for Cisco Spark API
    -> Authorization        -- ^ Authorization string against Spark API.
    -> createParams         -- ^ One of 'CreatePerson', 'CreateRoom', 'CreateMembership', 'CreateMessage',
                            --   'CreateTeam' and 'CreateTeamMembership'.
    -> m (Response (ToCreateResponse createParams))
createEntity base auth param = httpJSON $ makeCommonCreateReq base auth (createPath param) param

-- | Create a Spark entity with given parameters.  A Left value will be returned on an JSON parse errors.
createEntityEither :: (MonadIO m, SparkCreate createParams, ToJSON createParams)
    => CiscoSparkRequest
    -> Authorization
    -> createParams
    -> m (Response (Either JSONException (ToCreateResponse createParams)))
createEntityEither base auth param = httpJSONEither $ makeCommonCreateReq base auth (createPath param) param

