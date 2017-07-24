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
    , addAuthorizationHeader
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









newtype Authorization = Authorization ByteString deriving (Eq, Show)

makeReqPath :: ByteString -> ByteString
makeReqPath path = "/v1/" <> path

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


streamTeamList :: MonadIO m => Request -> Source m Team
streamTeamList base = do
    let req = makeCommonListReq base "teams"
    res <- httpJSON req
    let (TeamList teams) = getResponseBody res
    yieldMany teams
    streamRestTeamList base res

streamRestTeamList :: MonadIO m => Request -> Response TeamList -> Source m Team
streamRestTeamList base res = do
    case getNextUrl res of
        Nothing     -> pure ()
        Just url    -> do
            let auth = head $ getRequestHeader "Authorization" base
            let maybeNextReq = parseRequest $ "GET " <> (C8.unpack url)
            case maybeNextReq of
                Nothing         -> pure ()
                Just nextReq    -> do
                    nextRes <- httpJSON $ addRequestHeader "Authorization" auth $ nextReq
                    let (TeamList teams) = getResponseBody nextRes
                    yieldMany teams
                    streamRestTeamList base nextRes



makeCommonDetailReq
    :: Request      -- ^ Common request components
    -> ByteString   -- ^ API category part of REST URL path
    -> Text         -- ^ Identifier string part of REST URL path
    -> Request
makeCommonDetailReq base path idStr = setRequestPath ("/v1/" <> path <> "/" <> encodeUtf8 idStr)
                                    $ setRequestMethod "GET"
                                    $ base

getTeamDetail :: MonadIO m => Request -> TeamId -> m (Response Team)
getTeamDetail base (TeamId idStr) = httpJSON $ makeCommonDetailReq base "teams" idStr

getTeamDetailEither :: MonadIO m => Request -> TeamId -> m (Response (Either JSONException Team))
getTeamDetailEither base (TeamId idStr) = httpJSONEither $ makeCommonDetailReq base "teams" idStr

getPersonDetail :: MonadIO m => Request -> PersonId -> m (Response Person)
getPersonDetail base (PersonId idStr) = httpJSON $ makeCommonDetailReq base "people" idStr

getPersonDetailEither :: MonadIO m => Request -> PersonId -> m (Response (Either JSONException Person))
getPersonDetailEither base (PersonId idStr) = httpJSONEither $ makeCommonDetailReq base "people" idStr




