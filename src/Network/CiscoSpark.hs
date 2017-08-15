{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.CiscoSpark
    (
    -- * Types
      Authorization (..)
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
    , ciscoSparkBaseRequest
    ) where

import           Conduit
import           Data.Aeson                  (FromJSON)
import           Data.ByteString             (ByteString)
import           Data.ByteString.Char8       as C8 (unpack)
import           Data.Maybe                  (maybeToList)
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text)
import           Data.Text.Encoding          (encodeUtf8)
import           Network.HTTP.Simple

import           Network.CiscoSpark.Internal
import           Network.CiscoSpark.Types








-- | Authorization string against Spark API to be contained in HTTP Authorization header of every request.
newtype Authorization = Authorization ByteString deriving (Eq, Show)

-- | Common part of 'Request' against Spark API.
ciscoSparkBaseRequest :: Request
ciscoSparkBaseRequest
    = addRequestHeader "Content-Type" "application/json; charset=utf-8"
    $ setRequestPort 443
    $ setRequestHost "api.ciscospark.com"
    $ setRequestSecure True
    $ defaultRequest

-- | Add given Authorization into request header.
addAuthorizationHeader :: Authorization -> Request -> Request
addAuthorizationHeader (Authorization auth) = addRequestHeader "Authorization" ("Bearer " <> auth)

-- | Building common part of 'Request' for List APIs.
makeCommonListReq
    :: Request      -- ^ Common request components
    -> ByteString   -- ^ API category part of REST URL path
    -> Request
makeCommonListReq base path = setRequestPath ("/v1/" <> path)
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
streamPersonList :: MonadIO m => Authorization -> Request -> PersonQuery -> Source m Person
streamPersonList auth base query = do
    let email       = maybeToList $ (\(Email e) -> ("email", Just (encodeUtf8 e))) <$> personQueryEmail query
        displayName = maybeToList $ (\(DisplayName n) -> ("displayName", Just (encodeUtf8 n))) <$> personQueryDisplayName query
        orgId       = maybeToList $ (\(OrganizationId o) -> ("orgId", Just (encodeUtf8 o))) <$> personQueryOrgId query
        queryList   = email <> displayName <> orgId
    streamList auth $ setRequestQueryString queryList $ makeCommonListReq base "people"

-- | Query list of 'Room' and stream it into Conduit pipe.  It automatically performs pagination.
streamRoomList :: MonadIO m => Authorization -> Request -> RoomQuery -> Source m Room
streamRoomList auth base query = do
    let teamId    = maybeToList $ (\(TeamId e) -> ("teamId", Just (encodeUtf8 e))) <$> roomQueryTeamId query
        roomType  = maybeToList $ (\t -> ("type", Just $ roomTypeToQueryString t)) <$> roomQueryRoomType query
        sortBy    = maybeToList $ (\o -> ("sortBy", Just $ roomQuerySortByToQueryString o)) <$> roomQuerySortBy query
        queryList = teamId <> roomType <> sortBy
    streamList auth $ setRequestQueryString queryList $ makeCommonListReq base "rooms"

-- | Query list of 'Membership' and stream it into Conduit pipe.  It automatically performs pagination.
streamMembershipList :: MonadIO m => Authorization -> Request -> MembershipQuery -> Source m Membership
streamMembershipList auth base query = do
    let roomId    = maybeToList $ (\(RoomId r) -> ("roomId", Just (encodeUtf8 r))) <$> membershipQueryRoomId query
        personId  = maybeToList $ (\(PersonId p) -> ("personId", Just (encodeUtf8 p))) <$> membershipQueryPersonId query
        email     = maybeToList $ (\(Email e) -> ("personEmail", Just (encodeUtf8 e))) <$> membershipQueryPersonEmail query
        queryList = roomId <> personId <> email
    streamList auth $ setRequestQueryString queryList $ makeCommonListReq base "memberships"



-- | Query list of 'Team' and stream it into Conduit pipe.  It automatically performs pagination.
streamTeamList :: MonadIO m => Authorization -> Request -> Source m Team
streamTeamList auth base = streamList auth $ makeCommonListReq base "teams"

-- | Query list of 'TeamMembership' and stream it into Conduit pipe.  It automatically performs pagination.
streamTeamMembershipList :: MonadIO m => Authorization -> Request -> TeamMembershipQuery -> Source m TeamMembership
streamTeamMembershipList auth base query = do
    let queryList = maybeToList $ (\(TeamId t) -> ("teamId", Just (encodeUtf8 t))) <$> teamMembershipQueryTeamId query
    streamList auth $ setRequestQueryString queryList $ makeCommonListReq base "team/memberships"

-- | Query list of 'Organization' and stream it into Conduit pipe.  It automatically performs pagination.
streamOrganizationList :: MonadIO m => Authorization -> Request -> Source m Organization
streamOrganizationList auth base = streamList auth $ makeCommonListReq base "organizations"

-- | Query list of 'License' and stream it into Conduit pipe.  It automatically performs pagination.
streamLicenseList :: MonadIO m => Authorization -> Request -> LicenseQuery -> Source m License
streamLicenseList auth base query = do
    let queryList = maybeToList $ (\(OrganizationId o) -> ("orgId", Just (encodeUtf8 o))) <$> licenseQueryOrgId query
    streamList auth $ setRequestQueryString queryList $ makeCommonListReq base "licenses"

-- | Query list of 'Role' and stream it into Conduit pipe.  It automatically performs pagination.
streamRoleList :: MonadIO m => Authorization -> Request -> Source m Role
streamRoleList auth base = streamList auth $ makeCommonListReq base "roles"


makeCommonDetailReq
    :: Request          -- ^ Common request components.
    -> Authorization    -- ^ Authorization string against Spark API.
    -> ByteString       -- ^ API category part of REST URL path.
    -> Text             -- ^ Identifier string part of REST URL path.
    -> Request
makeCommonDetailReq base auth path idStr
    = setRequestPath ("/v1/" <> path <> "/" <> encodeUtf8 idStr)
    $ setRequestMethod "GET"
    $ addAuthorizationHeader auth
    $ base

-- | Get details for 'Person' by ID.  A JSONException runtime exception will be thrown on an JSON parse errors.
getPersonDetail :: MonadIO m => Request -> Authorization -> PersonId -> m (Response Person)
getPersonDetail base auth (PersonId idStr) = httpJSON $ makeCommonDetailReq base auth "people" idStr

-- | Get details for 'Person' by ID.  A Left value will be returned on an JSON parse errors.
getPersonDetailEither :: MonadIO m => Request -> Authorization -> PersonId -> m (Response (Either JSONException Person))
getPersonDetailEither base auth (PersonId idStr) = httpJSONEither $ makeCommonDetailReq base auth "people" idStr

-- | Get details for 'Room' by ID.  A JSONException runtime exception will be thrown on an JSON parse errors.
getRoomDetail :: MonadIO m => Request -> Authorization -> RoomId -> m (Response Room)
getRoomDetail base auth (RoomId idStr) = httpJSON $ makeCommonDetailReq base auth "rooms" idStr

-- | Get details for 'Room' by ID.  A Left value will be returned on an JSON parse errors.
getRoomDetailEither :: MonadIO m => Request -> Authorization -> RoomId -> m (Response (Either JSONException Room))
getRoomDetailEither base auth (RoomId idStr) = httpJSONEither $ makeCommonDetailReq base auth "rooms" idStr

-- | Get details for 'Membership' by ID.  A JSONException runtime exception will be thrown on an JSON parse errors.
getMembershipDetail :: MonadIO m => Request -> Authorization -> MembershipId -> m (Response Membership)
getMembershipDetail base auth (MembershipId idStr) = httpJSON $ makeCommonDetailReq base auth "memberships" idStr

-- | Get details for 'Membership' by ID.  A Left value will be returned on an JSON parse errors.
getMembershipDetailEither :: MonadIO m => Request -> Authorization -> MembershipId -> m (Response (Either JSONException Membership))
getMembershipDetailEither base auth (MembershipId idStr) = httpJSONEither $ makeCommonDetailReq base auth "memberships" idStr



-- | Get details for 'Team' by ID.  A JSONException runtime exception will be thrown on an JSON parse errors.
getTeamDetail :: MonadIO m => Request -> Authorization -> TeamId -> m (Response Team)
getTeamDetail base auth (TeamId idStr) = httpJSON $ makeCommonDetailReq base auth "teams" idStr

-- | Get details for 'Team' by ID.  A Left value will be returned on an JSON parse errors.
getTeamDetailEither :: MonadIO m => Request -> Authorization -> TeamId -> m (Response (Either JSONException Team))
getTeamDetailEither base auth (TeamId idStr) = httpJSONEither $ makeCommonDetailReq base auth "teams" idStr

-- | Get details for 'TeamMembership' by ID.  A JSONException runtime exception will be thrown on an JSON parse errors.
getTeamMembershipDetail :: MonadIO m => Request -> Authorization -> TeamMembershipId -> m (Response TeamMembership)
getTeamMembershipDetail base auth (TeamMembershipId idStr) = httpJSON $ makeCommonDetailReq base auth "team/memberships" idStr

-- | Get details for 'TeamMembership' by ID.  A Left value will be returned on an JSON parse errors.
getTeamMembershipDetailEither :: MonadIO m => Request -> Authorization -> TeamMembershipId -> m (Response (Either JSONException TeamMembership))
getTeamMembershipDetailEither base auth (TeamMembershipId idStr) = httpJSONEither $ makeCommonDetailReq base auth "team/memberships" idStr

-- | Get details for 'Organization' by ID.  A JSONException runtime exception will be thrown on an JSON parse errors.
getOrganizationDetail :: MonadIO m => Request -> Authorization -> OrganizationId -> m (Response Organization)
getOrganizationDetail base auth (OrganizationId idStr) = httpJSON $ makeCommonDetailReq base auth "organizations" idStr

-- | Get details for 'Organization' by ID.  A Left value will be returned on an JSON parse errors.
getOrganizationDetailEither :: MonadIO m => Request -> Authorization -> OrganizationId -> m (Response (Either JSONException Organization))
getOrganizationDetailEither base auth (OrganizationId idStr) = httpJSONEither $ makeCommonDetailReq base auth "organizations" idStr

-- | Get details for 'License' by ID.  A JSONException runtime exception will be thrown on an JSON parse errors.
getLicenseDetail :: MonadIO m => Request -> Authorization -> LicenseId -> m (Response License)
getLicenseDetail base auth (LicenseId idStr) = httpJSON $ makeCommonDetailReq base auth "licenses" idStr

-- | Get details for 'License' by ID.  A Left value will be returned on an JSON parse errors.
getLicenseDetailEither :: MonadIO m => Request -> Authorization -> LicenseId -> m (Response (Either JSONException License))
getLicenseDetailEither base auth (LicenseId idStr) = httpJSONEither $ makeCommonDetailReq base auth "licenses" idStr

-- | Get details for 'Role' by ID.  A JSONException runtime exception will be thrown on an JSON parse errors.
getRoleDetail :: MonadIO m => Request -> Authorization -> RoleId -> m (Response Role)
getRoleDetail base auth (RoleId idStr) = httpJSON $ makeCommonDetailReq base auth "roles" idStr

-- | Get details for 'Role' by ID.  A Left value will be returned on an JSON parse errors.
getRoleDetailEither :: MonadIO m => Request -> Authorization -> RoleId -> m (Response (Either JSONException Role))
getRoleDetailEither base auth (RoleId idStr) = httpJSONEither $ makeCommonDetailReq base auth "roles" idStr
