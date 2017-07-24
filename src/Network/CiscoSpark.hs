{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.CiscoSpark
    (
    -- * Types
      Authorization (..)

    , Person (..)
    , PersonId (..)
    , Email (..)
    , DisplayName (..)
    , NickName (..)
    , FirstName (..)
    , LastName (..)
    , AvatarUrl (..)
    , OrganizationId (..)
    , RoleId (..)
    , LicenseId (..)
    , Timezone (..)
    , PersonStatus (..)
    , PersonType (..)
    , PersonList (..)
    , CreatePerson (..)
    , UpdatePerson (..)

    , Room (..)
    , RoomId (..)
    , RoomTitle (..)
    , RoomType (..)
    , RoomList (..)
    , CreateRoom (..)
    , UpdateRoom (..)

    , Membership (..)
    , MembershipId (..)
    , MembershipList (..)
    , CreateMembership (..)
    , UpdateMembership (..)

    , Message (..)
    , MessageId (..)
    , MessageText (..)
    , MessageHtml (..)
    , MessageMarkdown (..)
    , FileUrl (..)
    , MessageList (..)
    , CreateMessage (..)

    , TeamName (..)
    , TeamId (..)
    , Team (..)
    , TeamList (..)
    , CreateTeam (..)
    , UpdateTeam (..)

    , TeamMembership (..)
    , TeamMembershipId (..)
    , TeamMembershipList (..)
    , CreateTeamMembership (..)
    , UpdateTeamMembership (..)

    , Organization (..)
    , OrganizationDisplayName (..)
    , OrganizationList (..)

    , License (..)
    , LicenseDisplayName (..)
    , LicenseUnit (..)
    , LicenseList (..)

    , Role (..)
    , RoleName (..)
    , RoleList (..)

    , Timestamp (..)
    -- * Functions
    , getPersonDetail
    , getPersonDetailEither
    , streamTeamList
    , getTeamDetailEither
    , getTeamDetail
    , ciscoSparkBaseRequest
    ) where

import           Conduit
import           Data.Aeson                  (FromJSON)
import           Data.ByteString             (ByteString)
import           Data.ByteString.Char8        as C8 (unpack)
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text)
import           Data.Text.Encoding          (encodeUtf8)
import           Network.HTTP.Simple

import           Network.CiscoSpark.Internal
import           Network.CiscoSpark.Types








-- | Authorization string against Spark API to be contained in HTTP Authorization header of every request.
newtype Authorization = Authorization ByteString deriving (Eq, Show)

makeReqPath :: ByteString -> ByteString
makeReqPath path = "/v1/" <> path

-- | Common part of 'Request' against Spark API.
ciscoSparkBaseRequest :: Request
ciscoSparkBaseRequest
    = addRequestHeader "Content-Type" "application/json; charset=utf-8"
    $ setRequestPort 443
    $ setRequestHost "api.ciscospark.com"
    $ setRequestSecure True
    $ defaultRequest

addAuthorizationHeader :: Authorization -> Request -> Request
addAuthorizationHeader (Authorization auth) = addRequestHeader "Authorization" ("Bearer " <> auth)




hasNextRel :: [(LinkParam, ByteString)] -> Bool
hasNextRel = any (\(param, str) -> param == Rel && str == "next")

isNextRel :: LinkHeader -> Bool
isNextRel = hasNextRel . linkHeaderParams




makeCommonListReq
    :: Request      -- ^ Common request components
    -> ByteString   -- ^ API category part of REST URL path
    -> Request
makeCommonListReq base path = setRequestPath ("/v1/" <> path)
                            $ setRequestMethod "GET"
                            $ base


streamTeamList :: MonadIO m => Request -> Authorization -> Source m Team
streamTeamList base auth = do
    let req = addAuthorizationHeader auth $ makeCommonListReq base "teams"
    res <- httpJSON req
    let (TeamList teams) = getResponseBody res
    yieldMany teams
    streamRestTeamList base auth res

streamRestTeamList :: MonadIO m => Request -> Authorization -> Response TeamList -> Source m Team
streamRestTeamList base auth res = do
    case getNextUrl res of
        Nothing     -> pure ()
        Just url    -> do
            let maybeNextReq = parseRequest $ "GET " <> (C8.unpack url)
            case maybeNextReq of
                Nothing         -> pure ()
                Just nextReq    -> do
                    nextRes <- httpJSON $ addAuthorizationHeader auth $ nextReq
                    let (TeamList teams) = getResponseBody nextRes
                    yieldMany teams
                    streamRestTeamList base auth nextRes



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

getTeamDetail :: MonadIO m => Request -> Authorization -> TeamId -> m (Response Team)
getTeamDetail base auth (TeamId idStr) = httpJSON $ makeCommonDetailReq base auth "teams" idStr

getTeamDetailEither :: MonadIO m => Request -> Authorization -> TeamId -> m (Response (Either JSONException Team))
getTeamDetailEither base auth (TeamId idStr) = httpJSONEither $ makeCommonDetailReq base auth "teams" idStr

getPersonDetail :: MonadIO m => Request -> Authorization -> PersonId -> m (Response Person)
getPersonDetail base auth (PersonId idStr) = httpJSON $ makeCommonDetailReq base auth "people" idStr

getPersonDetailEither :: MonadIO m => Request -> Authorization -> PersonId -> m (Response (Either JSONException Person))
getPersonDetailEither base auth (PersonId idStr) = httpJSONEither $ makeCommonDetailReq base auth "people" idStr




