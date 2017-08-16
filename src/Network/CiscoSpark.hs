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
    -- ** People
    , streamPersonList
    , getPersonDetail
    , getPersonDetailEither
    -- ** Rooms
    , streamRoomList
    , getRoomDetail
    , getRoomDetailEither
    -- ** Memberships
    , streamMembershipList
    , getMembershipDetail
    , getMembershipDetailEither
    -- ** Messages
    , defaultMessageQuery
    , streamMessageList
    , getMessageDetail
    , getMessageDetailEither
    -- ** Teams
    , streamTeamList
    , getTeamDetailEither
    , getTeamDetail
    , streamTeamMembershipList
    -- ** Team Memberships
    , getTeamMembershipDetail
    , getTeamMembershipDetailEither
    -- ** Organizations
    , streamOrganizationList
    , getOrganizationDetail
    , getOrganizationDetailEither
    -- ** Licenses
    , streamLicenseList
    , getLicenseDetail
    , getLicenseDetailEither
    -- ** Roles
    , streamRoleList
    , getRoleDetail
    , getRoleDetailEither
    ) where

import           Conduit
import           Data.Aeson                  (FromJSON)
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

streamListLoop :: (MonadIO m, FromJSON a, SparkListItem i) => Authorization -> Response a -> Source m i
streamListLoop auth res = case getNextUrl res >>= (\url -> parseRequest $ "GET " <> (C8.unpack url)) of
    Nothing         -> pure ()
    Just nextReq    -> do
        nextRes <- httpJSON $ addAuthorizationHeader auth nextReq
        yieldMany . unwrap $ getResponseBody nextRes
        streamListLoop auth nextRes

-- | Query list of 'Person' and stream it into Conduit pipe.  It automatically performs pagination.
streamPersonList :: MonadIO m => Authorization -> CiscoSparkRequest -> PersonQuery -> Source m Person
streamPersonList auth base query = streamList auth $ setRequestQueryString qs $ makeCommonListReq base "people"
  where
    qs = catMaybes [ (\(Email e) -> ("email", Just (encodeUtf8 e))) <$> personQueryEmail query
                   , (\(DisplayName n) -> ("displayName", Just (encodeUtf8 n))) <$> personQueryDisplayName query
                   , (\(OrganizationId o) -> ("orgId", Just (encodeUtf8 o))) <$> personQueryOrgId query
                   ]

-- | Query list of 'Room' and stream it into Conduit pipe.  It automatically performs pagination.
streamRoomList :: MonadIO m => Authorization -> CiscoSparkRequest -> RoomQuery -> Source m Room
streamRoomList auth base query = streamList auth $ setRequestQueryString qs $ makeCommonListReq base "rooms"
  where
    qs = catMaybes [ (\(TeamId e) -> ("teamId", Just (encodeUtf8 e))) <$> roomQueryTeamId query
                   , (\t -> ("type", Just $ roomTypeToQueryString t)) <$> roomQueryRoomType query
                   , (\o -> ("sortBy", Just $ roomQuerySortByToQueryString o)) <$> roomQuerySortBy query
                   ]

-- | Query list of 'Membership' and stream it into Conduit pipe.  It automatically performs pagination.
streamMembershipList :: MonadIO m => Authorization -> CiscoSparkRequest -> MembershipQuery -> Source m Membership
streamMembershipList auth base query = streamList auth $ setRequestQueryString qs $ makeCommonListReq base "memberships"
  where
    qs = catMaybes [ (\(RoomId r) -> ("roomId", Just (encodeUtf8 r))) <$> membershipQueryRoomId query
                   , (\(PersonId p) -> ("personId", Just (encodeUtf8 p))) <$> membershipQueryPersonId query
                   , (\(Email e) -> ("personEmail", Just (encodeUtf8 e))) <$> membershipQueryPersonEmail query
                   ]

-- | Query list of 'Message' and stream it into Conduit pipe.  It automatically performs pagination.
streamMessageList :: MonadIO m => Authorization -> CiscoSparkRequest -> MessageQuery -> Source m Message
streamMessageList auth base query = streamList auth $ setRequestQueryString qs $ makeCommonListReq base "messages"
  where
    qs = ("roomId", Just $ encodeUtf8 rid) : catMaybes
        [ (\p -> ("mentionedPeople", Just $ mentionedPeopleToQueryString p)) <$> messageQueryMentionedPeople query
        , (\(Timestamp t) -> ("before", Just (encodeUtf8 t))) <$> messageQueryBefore query
        , (\(MessageId m) -> ("beforeMessage", Just (encodeUtf8 m))) <$> messageQueryBeforeMessage query
        ]
    (RoomId rid) = messageQueryRoomId query

-- | Query list of 'Team' and stream it into Conduit pipe.  It automatically performs pagination.
streamTeamList :: MonadIO m => Authorization -> CiscoSparkRequest -> Source m Team
streamTeamList auth base = streamList auth $ makeCommonListReq base "teams"

-- | Query list of 'TeamMembership' and stream it into Conduit pipe.  It automatically performs pagination.
streamTeamMembershipList :: MonadIO m => Authorization -> CiscoSparkRequest -> TeamMembershipQuery -> Source m TeamMembership
streamTeamMembershipList auth base query = streamList auth $ setRequestQueryString qs $ makeCommonListReq base "team/memberships"
  where
    qs = maybeToList $ (\(TeamId t) -> ("teamId", Just (encodeUtf8 t))) <$> teamMembershipQueryTeamId query

-- | Query list of 'Organization' and stream it into Conduit pipe.  It automatically performs pagination.
streamOrganizationList :: MonadIO m => Authorization -> CiscoSparkRequest -> Source m Organization
streamOrganizationList auth base = streamList auth $ makeCommonListReq base "organizations"

-- | Query list of 'License' and stream it into Conduit pipe.  It automatically performs pagination.
streamLicenseList :: MonadIO m => Authorization -> CiscoSparkRequest -> LicenseQuery -> Source m License
streamLicenseList auth base query = streamList auth $ setRequestQueryString qs $ makeCommonListReq base "licenses"
  where
    qs = maybeToList $ (\(OrganizationId o) -> ("orgId", Just (encodeUtf8 o))) <$> licenseQueryOrgId query

-- | Query list of 'Role' and stream it into Conduit pipe.  It automatically performs pagination.
streamRoleList :: MonadIO m => Authorization -> CiscoSparkRequest -> Source m Role
streamRoleList auth base = streamList auth $ makeCommonListReq base "roles"


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

-- | Get details for 'Person' by ID.  A JSONException runtime exception will be thrown on an JSON parse errors.
getPersonDetail :: MonadIO m => CiscoSparkRequest -> Authorization -> PersonId -> m (Response Person)
getPersonDetail base auth (PersonId idStr) = httpJSON $ makeCommonDetailReq base auth "people" idStr

-- | Get details for 'Person' by ID.  A Left value will be returned on an JSON parse errors.
getPersonDetailEither :: MonadIO m => CiscoSparkRequest -> Authorization -> PersonId -> m (Response (Either JSONException Person))
getPersonDetailEither base auth (PersonId idStr) = httpJSONEither $ makeCommonDetailReq base auth "people" idStr

-- | Get details for 'Room' by ID.  A JSONException runtime exception will be thrown on an JSON parse errors.
getRoomDetail :: MonadIO m => CiscoSparkRequest -> Authorization -> RoomId -> m (Response Room)
getRoomDetail base auth (RoomId idStr) = httpJSON $ makeCommonDetailReq base auth "rooms" idStr

-- | Get details for 'Room' by ID.  A Left value will be returned on an JSON parse errors.
getRoomDetailEither :: MonadIO m => CiscoSparkRequest -> Authorization -> RoomId -> m (Response (Either JSONException Room))
getRoomDetailEither base auth (RoomId idStr) = httpJSONEither $ makeCommonDetailReq base auth "rooms" idStr

-- | Get details for 'Membership' by ID.  A JSONException runtime exception will be thrown on an JSON parse errors.
getMembershipDetail :: MonadIO m => CiscoSparkRequest -> Authorization -> MembershipId -> m (Response Membership)
getMembershipDetail base auth (MembershipId idStr) = httpJSON $ makeCommonDetailReq base auth "memberships" idStr

-- | Get details for 'Membership' by ID.  A Left value will be returned on an JSON parse errors.
getMembershipDetailEither :: MonadIO m => CiscoSparkRequest -> Authorization -> MembershipId -> m (Response (Either JSONException Membership))
getMembershipDetailEither base auth (MembershipId idStr) = httpJSONEither $ makeCommonDetailReq base auth "memberships" idStr

-- | Get details for 'Message' by ID.  A JSONException runtime exception will be thrown on an JSON parse errors.
getMessageDetail :: MonadIO m => CiscoSparkRequest -> Authorization -> MessageId -> m (Response Message)
getMessageDetail base auth (MessageId idStr) = httpJSON $ makeCommonDetailReq base auth "messages" idStr

-- | Get details for 'Message' by ID.  A Left value will be returned on an JSON parse errors.
getMessageDetailEither :: MonadIO m => CiscoSparkRequest -> Authorization -> MessageId -> m (Response (Either JSONException Message))
getMessageDetailEither base auth (MessageId idStr) = httpJSONEither $ makeCommonDetailReq base auth "messages" idStr

-- | Get details for 'Team' by ID.  A JSONException runtime exception will be thrown on an JSON parse errors.
getTeamDetail :: MonadIO m => CiscoSparkRequest -> Authorization -> TeamId -> m (Response Team)
getTeamDetail base auth (TeamId idStr) = httpJSON $ makeCommonDetailReq base auth "teams" idStr

-- | Get details for 'Team' by ID.  A Left value will be returned on an JSON parse errors.
getTeamDetailEither :: MonadIO m => CiscoSparkRequest -> Authorization -> TeamId -> m (Response (Either JSONException Team))
getTeamDetailEither base auth (TeamId idStr) = httpJSONEither $ makeCommonDetailReq base auth "teams" idStr

-- | Get details for 'TeamMembership' by ID.  A JSONException runtime exception will be thrown on an JSON parse errors.
getTeamMembershipDetail :: MonadIO m => CiscoSparkRequest -> Authorization -> TeamMembershipId -> m (Response TeamMembership)
getTeamMembershipDetail base auth (TeamMembershipId idStr) = httpJSON $ makeCommonDetailReq base auth "team/memberships" idStr

-- | Get details for 'TeamMembership' by ID.  A Left value will be returned on an JSON parse errors.
getTeamMembershipDetailEither :: MonadIO m => CiscoSparkRequest -> Authorization -> TeamMembershipId -> m (Response (Either JSONException TeamMembership))
getTeamMembershipDetailEither base auth (TeamMembershipId idStr) = httpJSONEither $ makeCommonDetailReq base auth "team/memberships" idStr

-- | Get details for 'Organization' by ID.  A JSONException runtime exception will be thrown on an JSON parse errors.
getOrganizationDetail :: MonadIO m => CiscoSparkRequest -> Authorization -> OrganizationId -> m (Response Organization)
getOrganizationDetail base auth (OrganizationId idStr) = httpJSON $ makeCommonDetailReq base auth "organizations" idStr

-- | Get details for 'Organization' by ID.  A Left value will be returned on an JSON parse errors.
getOrganizationDetailEither :: MonadIO m => CiscoSparkRequest -> Authorization -> OrganizationId -> m (Response (Either JSONException Organization))
getOrganizationDetailEither base auth (OrganizationId idStr) = httpJSONEither $ makeCommonDetailReq base auth "organizations" idStr

-- | Get details for 'License' by ID.  A JSONException runtime exception will be thrown on an JSON parse errors.
getLicenseDetail :: MonadIO m => CiscoSparkRequest -> Authorization -> LicenseId -> m (Response License)
getLicenseDetail base auth (LicenseId idStr) = httpJSON $ makeCommonDetailReq base auth "licenses" idStr

-- | Get details for 'License' by ID.  A Left value will be returned on an JSON parse errors.
getLicenseDetailEither :: MonadIO m => CiscoSparkRequest -> Authorization -> LicenseId -> m (Response (Either JSONException License))
getLicenseDetailEither base auth (LicenseId idStr) = httpJSONEither $ makeCommonDetailReq base auth "licenses" idStr

-- | Get details for 'Role' by ID.  A JSONException runtime exception will be thrown on an JSON parse errors.
getRoleDetail :: MonadIO m => CiscoSparkRequest -> Authorization -> RoleId -> m (Response Role)
getRoleDetail base auth (RoleId idStr) = httpJSON $ makeCommonDetailReq base auth "roles" idStr

-- | Get details for 'Role' by ID.  A Left value will be returned on an JSON parse errors.
getRoleDetailEither :: MonadIO m => CiscoSparkRequest -> Authorization -> RoleId -> m (Response (Either JSONException Role))
getRoleDetailEither base auth (RoleId idStr) = httpJSONEither $ makeCommonDetailReq base auth "roles" idStr
