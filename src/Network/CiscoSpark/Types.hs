{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

module Network.CiscoSpark.Types where


import           GHC.Generics                (Generic)

import           Data.Aeson                  (FromJSON, ToJSON (..), parseJSON,
                                              withText)
import           Data.Aeson.Encoding         (string)
import           Data.Aeson.TH               (constructorTagModifier,
                                              defaultOptions, deriveJSON,
                                              fieldLabelModifier,
                                              omitNothingFields)
import           Data.ByteString             (ByteString)
import           Data.Default                (Default (def))
import           Data.Maybe                  (maybeToList, catMaybes)
import           Data.Text                   (Text)
import           Data.Text.Encoding          (encodeUtf8)

import           Network.CiscoSpark.Internal


-- | URL path for people API.
peoplePath :: ByteString
peoplePath = "people"

-- | URL path for rooms API.
roomsPath :: ByteString
roomsPath = "rooms"

-- | URL path for memberships API.
membershipsPath :: ByteString
membershipsPath = "memberships"

-- | URL path for messages API.
messagesPath :: ByteString
messagesPath = "messages"

-- | URL path for teams API.
teamsPath :: ByteString
teamsPath = "teams"

-- | URL path for team memberships API.
teamMembershipsPath :: ByteString
teamMembershipsPath = "team/memberships"

-- | URL path for organizations API.
organizationsPath :: ByteString
organizationsPath = "organizations"

-- | URL path for licenes API.
licensesPath :: ByteString
licensesPath = "licenses"

-- | URL path for roles API.
rolesPath :: ByteString
rolesPath = "roles"


{-|
    SparkListItem is a type class grouping types with following common usage.

    * It is used for return value of get-detail APIs.
    * It is used for element of return value of list APIs.

    SparkListItem also associates the above type to wrapping list type (e.g. associates 'Person' to 'PersonList').
    Wrapping type (PersonList in this case) is necessary for parsing JSON from REST API but what we are
    interested in is bare list such like [Person].  Type family association defined in this class
    is used for type translation from type of items to type of wrapper.
-}
class FromJSON (ToList i) => SparkListItem i where
    -- | Associate item type to wrapping list type.
    type ToList i :: *
    -- | Get bare list from wrapped type which can be parsed directly from JSON.
    unwrap :: ToList i -> [i]

-- class FromJSON (ToListResponse a) => SparkList a where
--     type ToListResponse a :: *
--     listPath :: a -> ByteString

class SparkApiPath a where
    apiPath :: a -> ByteString

class FromJSON (ToResponse a) => SparkResponse a where
    type ToResponse a :: *

class (SparkApiPath a, SparkResponse a) => SparkDetail a where
    toIdStr :: a -> Text

class (SparkApiPath a, SparkResponse a) => SparkFilter a where
    toFilterList :: a -> [(ByteString, Maybe ByteString)]

class (SparkApiPath a, SparkResponse a, ToJSON a) => SparkCreate a where

class (SparkApiPath a, SparkResponse a, ToJSON a) => SparkUpdate a where

-- | Type representing timestamp.  For now, it is just copied from API response JSON.
newtype Timestamp   = Timestamp Text deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- Person
-- | Identifying 'Person' describing detail of Cisco Spark user or bot.
newtype PersonId        = PersonId Text deriving (Eq, Show, Generic, ToJSON, FromJSON)
-- | Email address of user.
newtype Email           = Email Text deriving (Eq, Show, Generic, ToJSON, FromJSON)
-- | Display name of user.
newtype DisplayName     = DisplayName Text deriving (Eq, Show, Generic, ToJSON, FromJSON)
-- | Nickname of user.
newtype NickName        = NickName Text deriving (Eq, Show, Generic, ToJSON, FromJSON)
-- | First name of user.
newtype FirstName       = FirstName Text deriving (Eq, Show, Generic, ToJSON, FromJSON)
-- | Last name of user.
newtype LastName        = LastName Text deriving (Eq, Show, Generic, ToJSON, FromJSON)
-- | URL pointing to image file of Avatar.
newtype AvatarUrl       = AvatarUrl Text deriving (Eq, Show, Generic, ToJSON, FromJSON)
-- | 'Organization' identifier which user or team belongs to.
newtype OrganizationId  = OrganizationId Text deriving (Eq, Show, Generic, ToJSON, FromJSON)
-- | 'Role' identifier which can be assigned to user.  See 'Role' too.
newtype RoleId          = RoleId Text deriving (Eq, Show, Generic, ToJSON, FromJSON)
-- | 'License' identifier which can be enabled on user.  See 'License' too.
newtype LicenseId       = LicenseId Text deriving (Eq, Show, Generic, ToJSON, FromJSON)
-- | Timezone in timezone name.
newtype Timezone        = Timezone Text deriving (Eq, Show, Generic, ToJSON, FromJSON)

{-|
    Current status of 'Person'.
    It can be updated automatically by recent activity or explicitly updated by user's operation
    or propagated from vacation setting on email system.
-}
data PersonStatus   = PersonStatusActive        -- ^ The 'Person' is currently active.  Decoded from \"active\".
                    | PersonStatusInactive      -- ^ The 'Person' is currently not active.  Decoded from \"inactive\".
                    | PersonStatusOutOfOffice   -- ^ Email system of the 'Person' currently sets vacation.  Decoded from \"OutOfOffice\".
                    | PersonStatusDoNotDisturb  -- ^ The 'Person' is explicitly indicated do-not-disturb.  Decoded from \"DoNotDisturb\".
                    | PersonStatusUnknown       -- ^ The status of the 'Person' is unknown.  Decoded from \"unknown\".
                    deriving (Eq, Show, Generic)

-- | 'PersonStatus' implements 'toEncoding' to encode each constructor into JSON enum value.
instance ToJSON PersonStatus where
    toEncoding PersonStatusActive       = string "active"
    toEncoding PersonStatusInactive     = string "inactive"
    toEncoding PersonStatusOutOfOffice  = string "OutOfOffice"
    toEncoding PersonStatusDoNotDisturb = string "DoNotDisturb"
    toEncoding PersonStatusUnknown      = string "unknown"

-- | 'PersonStatus' implements 'parseJSON' to decode JSON enum value to a constructor.
instance FromJSON PersonStatus where
    parseJSON = withText "Pserson.Status" $ \s -> case s of
        "active"        -> pure PersonStatusActive
        "inactive"      -> pure PersonStatusInactive
        "OutOfOffice"   -> pure PersonStatusOutOfOffice
        "DoNotDisturb"  -> pure PersonStatusDoNotDisturb
        "unknown"       -> pure PersonStatusUnknown
        _               -> fail "Parsing Person.Status value failed: expected \"active\", \"inactive\", \"OutOfOffice\", \"DoNotDisturb\" or \"unknown\""

-- | 'PersonType' indicates whether the Person is real human or bot.
data PersonType     = PersonTypePerson  -- ^ The 'Person' is a real human.  Decoded from \"person\".
                    | PersonTypeBot     -- ^ The 'Person' is a bot.  Decoded from \"bot\".
                    deriving (Eq, Show)

$(deriveJSON defaultOptions { constructorTagModifier = dropAndLow 10 } ''PersonType)
-- ^ 'PersonType' derives ToJSON and FromJSON via deriveJSON template haskell function.

{-|
    'Person' is detail description of Cisco Spark user or bot.
    Person is decoded from response JSON of Get Person Details REST call.
    It is also element type of response of List People call.
-}
data Person = Person
    { personId            :: PersonId           -- ^ Identifier of the Person.
    , personEmails        :: [Email]            -- ^ List of email addresses which the Person has.
    , personDisplayName   :: DisplayName        -- ^ Display name of the Person.
    , personNickName      :: Maybe NickName     -- ^ Nickname of the Person.
    , personFirstName     :: Maybe FirstName    -- ^ First name of the Person.
    , personLastName      :: Maybe LastName     -- ^ Last name of the Person.
    , personAvatar        :: Maybe AvatarUrl    -- ^ URL pointing a image used for Avatar of the Person.
    , personOrgId         :: OrganizationId     -- ^ 'Organization' which the Person belongs to.
    , personRoles         :: Maybe [RoleId]     -- ^ List of roles assigned to the Person.
    , personLicenses      :: Maybe [LicenseId]  -- ^ List of licenses effective on the Person.
    , personCreated       :: Timestamp          -- ^ Timestamp when the Person was created.
    , personTimezone      :: Maybe Timezone     -- ^ Timezone of the Person.
    , personLastActivity  :: Maybe Timestamp    -- ^ Timestamp of the latest activity of the Person.
    , personStatus        :: Maybe PersonStatus -- ^ Current status of the Person
    , personInvitePending :: Maybe Bool         -- ^ True if invitation for the Person is pending.
    , personLoginEnabled  :: Maybe Bool         -- ^ True if login of the Person is enabled.
    , personType          :: Maybe PersonType   -- ^ Indicating if the Person is real human or bot.
    } deriving (Eq, Show)

$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLow 6, omitNothingFields = True } ''Person)
-- ^ 'Person' derives ToJSON and FromJSON via deriveJSON template haskell function.

-- | Get detail for a person API uses 'PersonId' and path "people".
instance SparkApiPath PersonId where
    apiPath _ = peoplePath

-- | Get detail for a person API uses "PersonId' and responses 'Person'.
instance SparkResponse PersonId where
    type ToResponse PersonId = Person

-- | User can get detail of a person.
instance SparkDetail PersonId where
    toIdStr (PersonId s) = s

-- | 'PersonList' is decoded from response JSON of List People REST call.  It is list of 'Person'.
newtype PersonList = PersonList { personListItems :: [Person] } deriving (Eq, Show)
$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLow 10, omitNothingFields = True } ''PersonList)
-- ^ 'PersonList' derives ToJSON and FromJSON via deriveJSON template haskell function.

-- | 'PersonList' wraps 'Person'.
instance SparkListItem Person where
    type ToList Person = PersonList
    unwrap = personListItems

-- | Optional query strings for people list API.
data PersonFilter = PersonFilter
    { personFilterEmail       :: Maybe Email             -- ^ Find person who has given email address.
    , personFilterDisplayName :: Maybe DisplayName       -- ^ Find person who has given display name.
    , personFilterOrgId       :: Maybe OrganizationId    -- ^ Find person who belongs to given organization.
    } deriving (Default, Eq, Generic, Show)

-- | List people API uses 'PersonFilter' and path "people".
instance SparkApiPath PersonFilter where
    apiPath _ = peoplePath

-- | List people API uses 'PersonFilter' and responses 'Person'.
instance SparkResponse PersonFilter where
    type ToResponse PersonFilter = Person

-- | User can list people with filter parameter.
instance SparkFilter PersonFilter where
    toFilterList filter = catMaybes
        [ (\(Email e) -> ("email", Just $ encodeUtf8 e)) <$> personFilterEmail filter
        , (\(DisplayName n) -> ("displayName", Just (encodeUtf8 n))) <$> personFilterDisplayName filter
        , (\(OrganizationId o) -> ("orgId", Just (encodeUtf8 o))) <$> personFilterOrgId filter
        ]

-- | 'CreatePerson' is encoded to request body JSON of Create a Person REST call.
data CreatePerson = CreatePerson
    { createPersonEmails      :: Maybe [Email]          -- ^ List of email addresses which the Person has.
    , createPersonDisplayName :: Maybe DisplayName      -- ^ Display name of the Person.
    , createPersonFirstName   :: Maybe FirstName        -- ^ First name of the Person.
    , createPersonLastName    :: Maybe LastName         -- ^ Last name of the Person.
    , createPersonAvatar      :: Maybe AvatarUrl        -- ^ URL pointing a image used for Avatar of the Person.
    , createPersonOrgId       :: Maybe OrganizationId   -- ^ Organization which the Person belongs to.
    , createPersonRoles       :: Maybe [RoleId]         -- ^ List of roles assigned to the person.
    , createPersonLicenses    :: Maybe [LicenseId]      -- ^ List of licenses effective on the Person.
    } deriving (Eq, Show)

$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLow 12, omitNothingFields = True } ''CreatePerson)
-- ^ 'CreatePerson' derives ToJSON and FromJSON via deriveJSON template haskell function.

-- | Create person API uses 'CreatePerson' and path "people".
instance SparkApiPath CreatePerson where
    apiPath _ = peoplePath

-- | Create person API uses "CreatePerson' and responses 'Person'.
instance SparkResponse CreatePerson where
    type ToResponse CreatePerson = Person

-- | User can create a person.
instance SparkCreate CreatePerson where

-- | 'UpdatePerson' is encoded to request body JSON of Update a Person REST call.
data UpdatePerson = UpdatePerson
    { updatePersonDisplayName :: Maybe DisplayName      -- ^ Display name of the Person.
    , updatePersonFirstName   :: Maybe FirstName        -- ^ First name of the Person.
    , updatePersonLastName    :: Maybe LastName         -- ^ Last name of the Person.
    , updatePersonAvatar      :: Maybe AvatarUrl        -- ^ URL pointing a image used for Avatar of the Person.
    , updatePersonOrgId       :: Maybe OrganizationId   -- ^ Organization which the Person belongs to.
    , updatePersonRoles       :: Maybe [RoleId]         -- ^ List of roles assigned to the person.
    , updatePersonLicenses    :: Maybe [LicenseId]      -- ^ List of licenses effective on the Person.
    } deriving (Eq, Show)

$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLow 12, omitNothingFields = True } ''UpdatePerson)
-- ^ 'UpdatePerson' derives ToJSON and FromJSON via deriveJSON template haskell function.

-- | Update person API uses 'UpdatePerson' and path "people".
instance SparkApiPath UpdatePerson where
    apiPath _ = peoplePath

-- | Update person API uses "UpdatePerson' and responses 'Person'.
instance SparkResponse UpdatePerson where
    type ToResponse UpdatePerson = Person

-- | User can update a person.
instance SparkUpdate UpdatePerson

-- | Identifying Team.
newtype TeamId      = TeamId Text deriving (Eq, Show, Generic, ToJSON, FromJSON)
-- | Name of Team
newtype TeamName    = TeamName Text deriving (Eq, Show, Generic, ToJSON, FromJSON)

{-|
    'Team' is group of 'Person' and group of 'Room'.
    A Person can belong to multiple Team but a Room can belong to at most one Team.
    Team is decoded from response JSON of Get Team Details REST call.
    It is also element type of response of List Teams call.
-}
data Team = Team
    { teamId        :: TeamId       -- ^ Identifier of the Team.
    , teamName      :: TeamName     -- ^ Name of the Team.
    , teamCreatorId :: PersonId     -- ^ Identifier of the Person who created the Team.
    , teamCreated   :: Timestamp    -- ^ Timestamp when the Team was created.
    } deriving (Eq, Show)

$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLow 4, omitNothingFields = True } ''Team)
-- ^ 'Team' derives ToJSON and FromJSON via deriveJSON template haskell function.

-- | Get detail for a team API uses 'TeamId' and path "teams".
instance SparkApiPath TeamId where
    apiPath _ = teamsPath

-- | Get detail for a team API uses "TeamId' and responses 'Team'.
instance SparkResponse TeamId where
    type ToResponse TeamId = Team

-- | User can get detail of a team.
instance SparkDetail TeamId where
    toIdStr (TeamId s) = s

-- | 'TeamList' is decoded from response JSON of List Teams REST call.  It is list of 'Team'.
newtype TeamList = TeamList { teamListItems :: [Team] } deriving (Eq, Show)
$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLow 8, omitNothingFields = True } ''TeamList)
-- ^ 'TeamList' derives ToJSON and FromJSON via deriveJSON template haskell function.

-- | 'TeamList' wraps 'Team'
instance SparkListItem Team where
    type ToList Team = TeamList
    unwrap = teamListItems

-- | 'CreateTeam' is encoded to request body JSON of Create a Team REST call.
newtype CreateTeam = CreateTeam { createTeamName :: TeamName } deriving (Eq, Show)
$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLow 10, omitNothingFields = True } ''CreateTeam)
-- ^ 'CreateTeam' derives ToJSON and FromJSON via deriveJSON template haskell function.

-- | Create team API uses 'CreateTeam' and path "teams".
instance SparkApiPath CreateTeam where
    apiPath _ = teamsPath

-- | Create team API uses "CreateTeam' and responses 'Team'.
instance SparkResponse CreateTeam where
    type ToResponse CreateTeam = Team

-- | User can create a team.
instance SparkCreate CreateTeam where

-- | 'UpdateTeam' is encoded to request body JSON of Update a Team REST call.
newtype UpdateTeam = UpdateTeam { updateTeamName :: TeamName } deriving (Eq, Show)
$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLow 10, omitNothingFields = True } ''UpdateTeam)
-- ^ 'UpdateTeam' derives ToJSON and FromJSON via deriveJSON template haskell function.

-- | Update team API uses 'UpdateTeam' and path "teams".
instance SparkApiPath UpdateTeam where
    apiPath _ = teamsPath

-- | Update team API uses "UpdateTeam' and responses 'Team'.
instance SparkResponse UpdateTeam where
    type ToResponse UpdateTeam = Team

-- | User can update a team.
instance SparkUpdate UpdateTeam


-- | Identifying TeamMembership.
newtype TeamMembershipId    = TeamMembershipId Text deriving (Eq, Show, Generic, ToJSON, FromJSON)

{-|
    'TeamMembership' is association between 'Team' and 'Person'.
    It can be N:N relation.  A Person can belong to multiple Team.
    TeamMembership is decoded from response JSON of Get Team Membership Details REST call.
    It is also element type of response of List Team Memberships call.
-}
data TeamMembership = TeamMembership
    { teamMembershipId                :: TeamMembershipId   -- ^ Identifier of the TeamMembership entry.
    , teamMembershipTeamId            :: TeamId             -- ^ Identifier of the 'Team' which the Person belongs to.
    , teamMembershipPersonId          :: PersonId           -- ^ Identifier of user who belongs to the Team.
    , teamMembershipPersonEmail       :: Email              -- ^ Email address of the user identified by the PersonId.
    , teamMembershipPersonDisplayName :: DisplayName        -- ^ Display name of the user identified by the PersonId.
    , teamMembershipPersonOrgId       :: OrganizationId     -- ^ Identifier of 'Organization' which the Team blongs to.
    , teamMembershipIsModerator       :: Bool               -- ^ The Person is moderator of the Team when True.
    , teamMembershipCreated           :: Timestamp          -- ^ Timestamp when the TeamMembership entry created.
    } deriving (Eq, Show)

$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLow 14, omitNothingFields = True } ''TeamMembership)
-- ^ 'TeamMembership' derives ToJSON and FromJSON via deriveJSON template haskell function.

-- | Get detail for a team membership API uses 'TeamMembershipId' and path "team/memberships".
instance SparkApiPath TeamMembershipId where
    apiPath _ = teamMembershipsPath

-- | Get detail for a team membership API uses "TeamMembershipId' and responses 'TeamMembership'.
instance SparkResponse TeamMembershipId where
    type ToResponse TeamMembershipId = TeamMembership

-- | User can get detail of a team membership.
instance SparkDetail TeamMembershipId where
    toIdStr (TeamMembershipId s) = s

-- | 'TeamMembershipList' is decoded from response JSON of List Team Memberships REST call.  It is list of 'TeamMembership'.
newtype TeamMembershipList = TeamMembershipList { teamMembershipListItems :: [TeamMembership] } deriving (Eq, Show)
$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLow 18, omitNothingFields = True } ''TeamMembershipList)
-- ^ 'TeamMembershipList' derives ToJSON and FromJSON via deriveJSON template haskell function.

-- | 'TeamMembershipList' wraps 'TeamMembership'
instance SparkListItem TeamMembership where
    type ToList TeamMembership = TeamMembershipList
    unwrap = teamMembershipListItems

-- | Optional query strings for team membership list API
newtype TeamMembershipFilter = TeamMembershipFilter
    { teamMembershipFilterTeamId :: Maybe TeamId -- ^ List membership only in given team.
    } deriving (Default, Eq, Generic, Show)

-- | List team memberships API uses 'TeamMembershipFilter' and path "team/memberships".
instance SparkApiPath TeamMembershipFilter where
    apiPath _ = teamMembershipsPath

-- | List team memberships API uses 'TeamMembershipFilter' and responses 'TeamMembership'.
instance SparkResponse TeamMembershipFilter where
    type ToResponse TeamMembershipFilter = TeamMembership

-- | User can list team membership with filter parameter.
instance SparkFilter TeamMembershipFilter where
    toFilterList filter = maybeToList $ (\(TeamId t) -> ("teamId", Just $ encodeUtf8 t)) <$> teamMembershipFilterTeamId filter

-- | 'CreateTeamMembership' is encoded to request body JSON of Create a Team Membership REST call.
data CreateTeamMembership = CreateTeamMembership
    { createTeamMembershipTeamId      :: TeamId         -- ^ Identifier of 'Team' which the user will be added to.
    , createTeamMembershipPersonId    :: Maybe PersonId -- ^ Identifier of 'Person' who will be added to the Team.
    , createTeamMembershipPersonEmail :: Maybe Email    -- ^ Email of the Person to be added.
    , createTeamMembershipIsModerator :: Maybe Bool     -- ^ The user becomes a moderator of the team if True.
    } deriving (Eq, Show)

$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLow 20, omitNothingFields = True } ''CreateTeamMembership)
-- ^ 'CreateTeamMembership' derives ToJSON and FromJSON via deriveJSON template haskell function.

-- | Create teamMembership API uses 'CreateTeamMembership' and path "team/memberships".
instance SparkApiPath CreateTeamMembership where
    apiPath _ = teamMembershipsPath

-- | Create teamMembership API uses "CreateTeamMembership' and responses 'TeamMembership'.
instance SparkResponse CreateTeamMembership where
    type ToResponse CreateTeamMembership = TeamMembership

-- | User can create a teamMembership.
instance SparkCreate CreateTeamMembership where

-- | 'UpdateTeamMembership' is encoded to request body JSON of Update a Team Membership REST call.
newtype UpdateTeamMembership = UpdateTeamMembership { updateTeamMembershipIsModerator :: Bool } deriving (Eq, Show)
$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLow 20, omitNothingFields = True } ''UpdateTeamMembership)
-- ^ 'UpdateTeamMembership' derives ToJSON and FromJSON via deriveJSON template haskell function.

-- | Update teamMembership API uses 'UpdateTeamMembership' and path "team/memberships".
instance SparkApiPath UpdateTeamMembership where
    apiPath _ = teamMembershipsPath

-- | Update teamMembership API uses "UpdateTeamMembership' and responses 'TeamMembership'.
instance SparkResponse UpdateTeamMembership where
    type ToResponse UpdateTeamMembership = TeamMembership

-- | User can update a teamMembership.
instance SparkUpdate UpdateTeamMembership


-- | Identifying 'Room'.
newtype RoomId      = RoomId Text deriving (Eq, Show, Generic, ToJSON, FromJSON)
-- | Title text of 'Room'.
newtype RoomTitle   = RoomTitle Text deriving (Eq, Show, Generic, ToJSON, FromJSON)
-- | SIP address.
newtype SipAddr     = SipAddr Text deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | 'RoomType' indicates if the 'Room' is for 1:1 user or group of users.
data RoomType   = RoomTypeDirect    -- ^ The Room is for 1:1.  Decoded from \"direct\".
                | RoomTypeGroup     -- ^ The Room is for group.  Decoded from \"group\".
                deriving (Eq, Show)

$(deriveJSON defaultOptions { constructorTagModifier = dropAndLow 8 } ''RoomType)
-- ^ 'RoomType' derives ToJSON and FromJSON via deriveJSON template haskell function.

{-|
    'Room' is communication space in Cisco Spark and called \"Space\" on UI.
    Historically it was called Room on UI too but UI has been changed to \"Space\" in order to avoid
    confusion with the concept \"Room\" associated to hardware facility of video conferencing on Spark.
    The name of Room is kept unchanged for backward compatibility.

    Room is decoded from response JSON of Get Room Details REST call.
    It is also element type of response of List Rooms call.
-}
data Room = Room
    { roomId           :: RoomId        -- ^ Identifier of the Room.
    , roomTitle        :: RoomTitle     -- ^ Title text of the Room.
    , roomType         :: RoomType      -- ^ Indicates if the Room is for 1:1 or group.
    , roomIsLocked     :: Bool          -- ^ True if the Room is locked.
    , roomSipAddress   :: Maybe SipAddr -- ^ SIP address of the Room.
    , roomLastActivity :: Timestamp     -- ^ Timestamp when the last activity was happen on the Room.
    , roomTeamId       :: Maybe TeamId  -- ^ Identifier of the 'Team' which the Room belongs to.
    , roomCreatorId    :: PersonId      -- ^ Identifier of 'Person' who created the Room.
    , roomCreated      :: Timestamp     -- ^ Timestamp when the Room was created.
    } deriving (Eq, Show)

$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLow 4, omitNothingFields = True } ''Room)
-- ^ 'Room' derives ToJSON and FromJSON via deriveJSON template haskell function.

-- | Get detail for a room API uses 'RoomId' and path "rooms".
instance SparkApiPath RoomId where
    apiPath _ = roomsPath

-- | Get detail for a room API uses "RoomId' and responses 'Room'.
instance SparkResponse RoomId where
    type ToResponse RoomId = Room

-- | User can get detail of a room.
instance SparkDetail RoomId where
    toIdStr (RoomId s) = s

-- | 'RoomList' is decoded from response JSON of List Rooms REST call.  It is list of 'Room'.
newtype RoomList = RoomList { roomListItems :: [Room] } deriving (Eq, Show)
$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLow 8, omitNothingFields = True } ''RoomList)
-- ^ 'RoomList' derives ToJSON and FromJSON via deriveJSON template haskell function.

-- | 'RoomList' wraps 'Room'
instance SparkListItem Room where
    type ToList Room = RoomList
    unwrap = roomListItems

data RoomFilterSortBy = RoomFilterSortById | RoomFilterSortByLastActivity | RoomFilterSortByCreated deriving (Eq, Show)

-- | Optional query strings for room list API
data RoomFilter = RoomFilter
    { roomFilterTeamId   :: Maybe TeamId             -- ^ List rooms only in given team.
    , roomFilterRoomType :: Maybe RoomType           -- ^ List given type rooms only.
    , roomFilterSortBy   :: Maybe RoomFilterSortBy    -- ^ Sort response by given option.
    } deriving (Default, Eq, Generic, Show)

-- | Sum type to ByteString converter for 'RoomType'.
roomTypeToFilterString :: RoomType -> ByteString
roomTypeToFilterString RoomTypeDirect = "direct"
roomTypeToFilterString RoomTypeGroup  = "group"

-- | Sum type to ByteString converter for 'RoomFilterSortBy'.
roomFilterSortByToFilterString :: RoomFilterSortBy -> ByteString
roomFilterSortByToFilterString RoomFilterSortById           = "id"
roomFilterSortByToFilterString RoomFilterSortByLastActivity = "lastactivity"
roomFilterSortByToFilterString RoomFilterSortByCreated      = "created"

-- | List rooms API uses 'RoomFilter' and path "rooms".
instance SparkApiPath RoomFilter where
    apiPath _ = roomsPath

-- | List rooms API uses 'RoomFilter' and responses 'Room'.
instance SparkResponse RoomFilter where
    type ToResponse RoomFilter = Room

-- | User can list rooms with filter parameter.
instance SparkFilter RoomFilter where
    toFilterList filter = catMaybes
        [ (\(TeamId e) -> ("teamId", Just $ encodeUtf8 e)) <$> roomFilterTeamId filter
        , (\t -> ("type", Just $ roomTypeToFilterString t)) <$> roomFilterRoomType filter
        , (\o -> ("sortBy", Just $ roomFilterSortByToFilterString o)) <$> roomFilterSortBy filter
        ]

-- | 'CreateRoom' is encoded to request body JSON of Create a Room REST call.
data CreateRoom = CreateRoom
    { createRoomTitle  :: RoomTitle     -- ^ Title text of newly created Room.
    , createRoomTeamId :: Maybe TeamId  -- ^ Identifier of 'Team' which the Room will belong to.  If Nothing, the new Room will be standalone.
    } deriving (Eq, Show)

$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLow 10, omitNothingFields = True } ''CreateRoom)
-- ^ 'CreateRoom' derives ToJSON and FromJSON via deriveJSON template haskell function.

-- | Create room API uses 'CreateRoom' and path "rooms".
instance SparkApiPath CreateRoom where
    apiPath _ = roomsPath

-- | Create room API uses "CreateRoom' and responses 'Room'.
instance SparkResponse CreateRoom where
    type ToResponse CreateRoom = Room

-- | User can create a room.
instance SparkCreate CreateRoom where

-- | 'UpdateRoom' is encoded to request body JSON of Update a Room REST call.
newtype UpdateRoom = UpdateRoom { updateRoomTitle :: RoomTitle } deriving (Eq, Show)
$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLow 10, omitNothingFields = True } ''UpdateRoom)
-- ^ 'UpdateRoom' derives ToJSON and FromJSON via deriveJSON template haskell function.

-- | Update room API uses 'UpdateRoom' and path "rooms".
instance SparkApiPath UpdateRoom where
    apiPath _ = roomsPath

-- | Update room API uses "UpdateRoom' and responses 'Room'.
instance SparkResponse UpdateRoom where
    type ToResponse UpdateRoom = Room

-- | User can update a room.
instance SparkUpdate UpdateRoom


-- | Identifying 'Membership'.
newtype MembershipId    = MembershipId Text deriving (Eq, Show, Generic, ToJSON, FromJSON)

{-|
    'Membership' is association between 'Room' and 'Person'.
    It can be N:N relation.  A Person can belong to multiple Room.
    Membership is decoded from response JSON of Get Membership Details REST call.
    It is also element type of response of List Memberships call.
-}
data Membership = Membership
    { membershipId                :: MembershipId   -- ^ Identifier of the Membership entry.
    , membershipRoomId            :: RoomId         -- ^ Identifier of the 'Room' associated to the Person
    , membershipPersonId          :: PersonId       -- ^ Identifier of the 'Person' associated to the Room
    , membershipPersonEmail       :: Email          -- ^ Email of the Person
    , membershipPersonDisplayName :: DisplayName    -- ^ Display name of the Person
    , membershipPersonOrgId       :: OrganizationId -- ^ Identifier of 'Organization' which the Person belongs to.
    , membershipIsModerator       :: Bool           -- ^ True if the Person is a moderator of the room.
    , membershipIsMonitor         :: Bool           -- ^ True if the Person is monitoring the Room.
    , membershipCreated           :: Timestamp      -- ^ Timestamp when the Membership was created.
    } deriving (Eq, Show)

$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLow 10, omitNothingFields = True } ''Membership)
-- ^ 'Membership' derives ToJSON and FromJSON via deriveJSON template haskell function.

-- | Get detail for a membership API uses 'MembershipId' and path "memberships".
instance SparkApiPath MembershipId where
    apiPath _ = membershipsPath

-- | Get detail for a membership API uses "MembershipId' and responses 'Membership'.
instance SparkResponse MembershipId where
    type ToResponse MembershipId = Membership

-- | User can get detail of a membership.
instance SparkDetail MembershipId where
    toIdStr (MembershipId s) = s

-- | 'MembershipList' is decoded from response JSON of List Memberships REST call.  It is list of 'Membership'.
newtype MembershipList = MembershipList { membershipListItems :: [Membership] } deriving (Eq, Show)
$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLow 14, omitNothingFields = True } ''MembershipList)
-- ^ 'MembershipList' derives ToJSON and FromJSON via deriveJSON template haskell function.

-- | 'MembershipList' wraps 'Membership'
instance SparkListItem Membership where
    type ToList Membership = MembershipList
    unwrap = membershipListItems

-- | Optional query strings for room membership list API
data MembershipFilter = MembershipFilter
    { membershipFilterRoomId      :: Maybe RoomId    -- ^ List membership only in given room.
    , membershipFilterPersonId    :: Maybe PersonId  -- ^ List membership related to given person of personId.
    , membershipFilterPersonEmail :: Maybe Email     -- ^ List membership related to given person of email.
    } deriving (Default, Eq, Generic, Show)

-- | List memberships API uses 'MembershipFilter' and path "memberships".
instance SparkApiPath MembershipFilter where
    apiPath _ = membershipsPath

-- | List memberships API uses 'MembershipFilter' and responses 'Membership'.
instance SparkResponse MembershipFilter where
    type ToResponse MembershipFilter = Membership

-- | User can list memberships with filter parameter.
instance SparkFilter MembershipFilter where
    toFilterList filter = catMaybes
        [ (\(RoomId r) -> ("roomId", Just $ encodeUtf8 r)) <$> membershipFilterRoomId filter
        , (\(PersonId p) -> ("personId", Just $ encodeUtf8 p)) <$> membershipFilterPersonId filter
        , (\(Email e) -> ("personEmail", Just $ encodeUtf8 e)) <$> membershipFilterPersonEmail filter
        ]

-- | 'CreateMembership' is encoded to request body JSON of Create a Membership REST call.
data CreateMembership = CreateMembership
    { createMembershipRoomId      :: RoomId         -- ^ Identifier of 'Room' which the Person will be added to.
    , createMembershipPersonId    :: Maybe PersonId -- ^ Identifier of 'Person' who will be added to the Room.
    , createMembershipPersonEmail :: Maybe Email    -- ^ Email of the Person to be added.
    , createMembershipIsModerator :: Maybe Bool     -- ^ The Person becomes a moderator of the Room if True.
    } deriving (Eq, Show)

$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLow 16, omitNothingFields = True } ''CreateMembership)
-- ^ 'CreateMembership' derives ToJSON and FromJSON via deriveJSON template haskell function.

-- | Create membership API uses 'CreateMembership' and path "memberships".
instance SparkApiPath CreateMembership where
    apiPath _ = membershipsPath

-- | Create membership API uses "CreateMembership' and responses 'Membership'.
instance SparkResponse CreateMembership where
    type ToResponse CreateMembership = Membership

-- | User can create a membership.
instance SparkCreate CreateMembership where

-- | 'UpdateMembership' is encoded to request body JSON of Update a Membership REST call.
newtype UpdateMembership = UpdateMembership { updateMembershipIsModerator :: Bool } deriving (Eq, Show)
$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLow 16, omitNothingFields = True } ''UpdateMembership)
-- ^ 'UpdateMembership' derives ToJSON and FromJSON via deriveJSON template haskell function.

-- | Update membership API uses 'UpdateMembership' and path "memberships".
instance SparkApiPath UpdateMembership where
    apiPath _ = membershipsPath

-- | Update membership API uses "UpdateMembership' and responses 'Membership'.
instance SparkResponse UpdateMembership where
    type ToResponse UpdateMembership = Membership

-- | User can update a membership.
instance SparkUpdate UpdateMembership


-- | Identifying 'Message'.
newtype MessageId   = MessageId Text deriving (Eq, Show, Generic, ToJSON, FromJSON)
-- | Body of message in plain text.
newtype MessageText = MessageText Text deriving (Eq, Show, Generic, ToJSON, FromJSON)
-- | Body of message in html.
newtype MessageHtml = MessageHtml Text deriving (Eq, Show, Generic, ToJSON, FromJSON)
-- | Body of message in markdown.
newtype MessageMarkdown = MessageMarkdown Text deriving (Eq, Show, Generic, ToJSON, FromJSON)
-- | URL pointing attached file of message.
newtype FileUrl     = FileUrl Text deriving (Eq, Show, Generic, ToJSON, FromJSON)

{-|
    'Message' is a message posted to a 'Room' by some 'Person'.
    Room is decoded from response JSON of Get Message Details REST call.
    It is also element type of response of List Messages call.
-}
data Message = Message
    { messageId              :: MessageId           -- ^ Identifier of the Message.
    , messageRoomId          :: RoomId              -- ^ Identifier of the room where the Message was sent.
    , messageRoomType        :: RoomType            -- ^ Type of Room the message was sent to.
    , messageToPersonId      :: Maybe PersonId      -- ^ Presents in documentation but doesn't appear in actual API response.
    , messageToPersonEmail   :: Maybe Email         -- ^ Presents in documentation but doesn't appear in actual API response.
    , messageText            :: MessageText         -- ^ Message body in plain text.
    , messageHtml            :: Maybe MessageHtml   -- ^ Message body in HTML.
    , messageFiles           :: Maybe [FileUrl]     -- ^ URL to files attached to the message.
    , messagePersonId        :: PersonId            -- ^ Identifier of 'Person' who sent the message.
    , messagePersonEmail     :: Email               -- ^ Email of Person who sent the message.
    , messageCreated         :: Timestamp           -- ^ Timestamp when the massage was sent.
    , messageMentionedPeople :: Maybe [PersonId]    -- ^ List of identifiers of Person were mentioned in the message.
    } deriving (Eq, Show)

$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLow 7, omitNothingFields = True } ''Message)
-- ^ 'Message' derives ToJSON and FromJSON via deriveJSON template haskell function.

-- | Get detail for message API uses 'MessageId' and path "messages".
instance SparkApiPath MessageId where
    apiPath _ = messagesPath

-- | Get detail for a message API uses "MessageId' and responses 'Message'.
instance SparkResponse MessageId where
    type ToResponse MessageId = Message

-- | User can get detail of a message.
instance SparkDetail MessageId where
    toIdStr (MessageId s) = s

-- | 'MessageList' is decoded from response JSON of List Messages REST call.  It is list of 'Message'.
newtype MessageList = MessageList { messageListItems :: [Message] } deriving (Eq, Show)
$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLow 11, omitNothingFields = True } ''MessageList)
-- ^ 'MessageList' derives ToJSON and FromJSON via deriveJSON template haskell function.

-- | 'MessageList' wraps 'Message'
instance SparkListItem Message where
    type ToList Message = MessageList
    unwrap = messageListItems

-- | Sum type for mentionedPeople query string.  It can be "me" or 'PersonId'.
data MentionedPeople = MentionedPeopleMe | MentionedPeople PersonId deriving (Eq, Show)

-- | Optional query strings for message list API
data MessageFilter = MessageFilter
    { messageFilterRoomId          :: RoomId                 -- ^ Mandatory parameter which room to search.
    , messageFilterMentionedPeople :: Maybe MentionedPeople  -- ^ List messages only mentioned to given person.
    , messageFilterBefore          :: Maybe Timestamp        -- ^ List messages posted before given timestamp.
    , messageFilterBeforeMessage   :: Maybe MessageId        -- ^ List messages posted before given message.
    } deriving (Eq, Show)

{-|
    Default value of query strings for message list API.
    Because 'RoomId' is mandatory, user have to supply it in order to get rest of defaults.
-}
defaultMessageFilter :: RoomId -> MessageFilter
defaultMessageFilter roomId = MessageFilter roomId Nothing Nothing Nothing

-- | Sum type to ByteString converter for mentionedPeople query string.
mentionedPeopleToFilterString :: MentionedPeople -> ByteString
mentionedPeopleToFilterString MentionedPeopleMe                     = "me"
mentionedPeopleToFilterString (MentionedPeople (PersonId personId)) = encodeUtf8 personId

-- | List messages API uses 'MessageFilter' and path "messages".
instance SparkApiPath MessageFilter where
    apiPath _ = messagesPath

-- | List messages API uses 'MessageFilter' and responses 'Message'.
instance SparkResponse MessageFilter where
    type ToResponse MessageFilter = Message

-- | User can list messages with filter parameter.
instance SparkFilter MessageFilter where
    toFilterList filter = ("roomId", Just $ encodeUtf8 rid) : catMaybes
        [ (\p -> ("mentionedPeople", Just $ mentionedPeopleToFilterString p)) <$> messageFilterMentionedPeople filter
        , (\(Timestamp t) -> ("before", Just $ encodeUtf8 t)) <$> messageFilterBefore filter
        , (\(MessageId m) -> ("beforeMessage", Just $ encodeUtf8 m)) <$> messageFilterBeforeMessage filter
        ]
      where
        (RoomId rid) = messageFilterRoomId filter

-- | 'CreateMessage' is encoded to request body JSON of Create a Message REST call.
data CreateMessage = CreateMessage
    { createMessageRoomId        :: Maybe RoomId            -- ^ Identifier of the 'Room' the message will be posted to.
    , createMessageToPersonId    :: Maybe PersonId          -- ^ Identifier of the 'Person' to whom the direct message will be sent.
    , createMessageToPersonEmail :: Maybe Email             -- ^ Email of Person who receives the Message.
    , createMessageText          :: Maybe MessageText       -- ^ Message body in plain text.
    , createMessageMarkdown      :: Maybe MessageMarkdown   -- ^ Message body in markdown format.
    , createMessageFiles         :: Maybe [FileUrl]         -- ^ URLs of Attached files to the message.
    } deriving (Eq, Show)

$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLow 13, omitNothingFields = True } ''CreateMessage)
-- ^ 'CreateMessage' derives ToJSON and FromJSON via deriveJSON template haskell function.

-- | Create message API uses 'CreateMessage' and path "messages".
instance SparkApiPath CreateMessage where
    apiPath _ = messagesPath

-- | Create message API uses "CreateMessage' and responses 'Message'.
instance SparkResponse CreateMessage where
    type ToResponse CreateMessage = Message

-- | User can create a message.
instance SparkCreate CreateMessage where


-- | Display name of 'Organization'
newtype OrganizationDisplayName  = OrganizationDisplayName Text deriving (Eq, Show, Generic, ToJSON, FromJSON)

{-|
    'Organization' is an administrative group of Cisco Spark users.
    Each 'Person' belongs to one Organization.
    Organization is decoded from response JSON of Get Organization Details REST call.
    It is also element type of response of List Organizations call.
-}
data Organization = Organization
    { organizationId          :: OrganizationId             -- ^ Identifier of the Organization.
    , organizationDisplayName :: OrganizationDisplayName    -- ^ Display name of the Organization.
    , organizationCreated     :: Timestamp                  -- ^ Timestamp when the Organization was created.
    } deriving (Eq, Show)

$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLow 12, omitNothingFields = True } ''Organization)
-- ^ 'Organization' derives ToJSON and FromJSON via deriveJSON template haskell function.

-- | Get detail for organization API uses 'OrganizationId' and path "organizations".
instance SparkApiPath OrganizationId where
    apiPath _ = organizationsPath

-- | Get detail for a organization API uses "OrganizationId' and responses 'Organization'.
instance SparkResponse OrganizationId where
    type ToResponse OrganizationId = Organization

-- | User can get detail of a organization.
instance SparkDetail OrganizationId where
    toIdStr (OrganizationId s) = s

-- | 'OrganizationList' is decoded from response JSON of List Organizations REST call.  It is list of 'Organization'.
newtype OrganizationList = OrganizationList { organizationListItems :: [Organization] } deriving (Eq, Show)
$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLow 16, omitNothingFields = True } ''OrganizationList)
-- ^ 'OrganizationList' derives ToJSON and FromJSON via deriveJSON template haskell function.

-- | 'OrganizationList' wraps 'Organization'
instance SparkListItem Organization where
    type ToList Organization = OrganizationList
    unwrap = organizationListItems


-- | Display name of License
newtype LicenseName  = LicenseName Text deriving (Eq, Show, Generic, ToJSON, FromJSON)
-- | Counting number of granted or consumed License
newtype LicenseUnit         = LicenseUnit Integer deriving (Eq, Show, Generic, ToJSON, FromJSON)

{-|
    'License' is allowance for features and services of Cisco Spark subscription.
    License is decoded from response JSON of Get License Details REST call.
    It is also element type of response of List Licenses call.
-}
data License = License
    { licenseId            :: LicenseId      -- ^ Identifier of the License.
    , licenseName          :: LicenseName    -- ^ Name of the License.
    , licenseTotalUnits    :: LicenseUnit    -- ^ Number of granted License.
    , licenseConsumedUnits :: LicenseUnit    -- ^ Number of currently consumed License.
    } deriving (Eq, Show)

$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLow 7, omitNothingFields = True } ''License)
-- ^ 'License' derives ToJSON and FromJSON via deriveJSON template haskell function.

-- | Get detail for license API uses 'LicenseId' and path "licenses".
instance SparkApiPath LicenseId where
    apiPath _ = licensesPath

-- | Get detail for a license API uses "LicenseId' and responses 'License'.
instance SparkResponse LicenseId where
    type ToResponse LicenseId = License

-- | User can get detail of a license.
instance SparkDetail LicenseId where
    toIdStr (LicenseId s) = s

-- | 'LicenseList' is decoded from response JSON of List Licenses REST call.  It is list of 'License'.
newtype LicenseList = LicenseList { licenseListItems :: [License] } deriving (Eq, Show)
$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLow 11, omitNothingFields = True } ''LicenseList)
-- ^ 'LicenseList' derives ToJSON and FromJSON via deriveJSON template haskell function.

-- | 'LicenseList' wraps 'License'
instance SparkListItem License where
    type ToList License = LicenseList
    unwrap = licenseListItems

-- | Optional query strings for license list API
newtype LicenseFilter = LicenseFilter
    { licenseFilterOrgId :: Maybe OrganizationId -- ^ List licenses only applicable to given organization.
    } deriving (Default, Eq, Generic, Show)

-- | List licenses API uses 'LicenseFilter' and path "licenses".
instance SparkApiPath LicenseFilter where
    apiPath _ = licensesPath

-- | List licenses API uses 'LicenseFilter' and responses 'License'.
instance SparkResponse LicenseFilter where
    type ToResponse LicenseFilter = License

-- | User can list licenses with filter parameter.
instance SparkFilter LicenseFilter where
    toFilterList filter = maybeToList $ (\(OrganizationId o) -> ("orgId", Just $ encodeUtf8 o)) <$> licenseFilterOrgId filter


-- | Name of 'Role'
newtype RoleName    = RoleName Text deriving (Eq, Show, Generic, ToJSON, FromJSON)

{-|
    A persona for an authenticated user, corresponding to a set of privileges within an organization.
    Role is decoded from response JSON of Get Role Details REST call.
    It is also element type of response of List Roles call.
-}
data Role = Role
    { roleId   :: RoleId    -- ^ Identifier of the Role
    , roleName :: RoleName  -- ^ Name of the Role
    } deriving (Eq, Show)

$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLow 4, omitNothingFields = True } ''Role)
-- ^ 'RoleName' derives ToJSON and FromJSON via deriveJSON template haskell function.

-- | Get detail for role API uses 'RoleId' and path "roles".
instance SparkApiPath RoleId where
    apiPath _ = rolesPath

-- | Get detail for a role API uses "RoleId' and responses 'Role'.
instance SparkResponse RoleId where
    type ToResponse RoleId = Role

-- | User can get detail of a role.
instance SparkDetail RoleId where
    toIdStr (RoleId s) = s

-- | 'RoleList' is decoded from response JSON of List Role REST call.  It is list of 'Role'.
newtype RoleList = RoleList { roleListItems :: [Role] } deriving (Eq, Show)
$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLow 8, omitNothingFields = True } ''RoleList)
-- ^ 'RoleList' derives ToJSON and FromJSON via deriveJSON template haskell function.

-- | 'RoleList' wraps 'Role'
instance SparkListItem Role where
    type ToList Role = RoleList
    unwrap = roleListItems
