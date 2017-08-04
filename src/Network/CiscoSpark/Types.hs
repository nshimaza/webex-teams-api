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
import           Data.Text                   (Text)
import           Data.Text.Encoding          (encodeUtf8)

import           Network.CiscoSpark.Internal

{-|
    SparkListItem is a type class grouping types with following common usage.

    * It is used for return value of get-detail APIs.
    * It is used for element of return value of list APIs.

    SparkListItem also associates the above type to wrapping list type (e.g. associates 'Person' to 'PersonList').
    Wrapping type (PersonList in this case) is necessary for parsing JSON from REST API but what we are
    interested in is bare list such like [Person].  Type family association defined in this class
    is used for type translation between type for item and type for wrapper.
-}
class FromJSON (ToList i) => SparkListItem i where
    -- | Associate item type to wrapping list type.
    type ToList i :: *
    -- | Get bare list from wrapped type which can be parsed directly from JSON.
    unwrap :: ToList i -> [i]

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
                    deriving (Eq, Show, Generic)

-- | 'PersonStatus' implements 'toEncoding' to encode each constructor into JSON enum value.
instance ToJSON PersonStatus where
    toEncoding PersonStatusActive       = string "active"
    toEncoding PersonStatusInactive     = string "inactive"
    toEncoding PersonStatusOutOfOffice  = string "OutOfOffice"
    toEncoding PersonStatusDoNotDisturb = string "DoNotDisturb"

-- | 'PersonStatus' implements 'parseJSON' to decode JSON enum value to a constructor.
instance FromJSON PersonStatus where
    parseJSON = withText "Pserson.Status" $ \s -> case s of
        "active"        -> pure PersonStatusActive
        "inactive"      -> pure PersonStatusInactive
        "OutOfOffice"   -> pure PersonStatusOutOfOffice
        "DoNotDisturb"  -> pure PersonStatusDoNotDisturb
        _               -> fail "Parsing Person.Status value failed: expected \"active\", \"inactive\", \"OutOfOffice\" or \"DoNotDisturb\""

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

-- | 'PersonList' is decoded from response JSON of List People REST call.  It is list of 'Person'.
newtype PersonList = PersonList { personListItems :: [Person] } deriving (Eq, Show)
$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLow 10, omitNothingFields = True } ''PersonList)
-- ^ 'PersonList' derives ToJSON and FromJSON via deriveJSON template haskell function.

instance SparkListItem Person where
    type ToList Person = PersonList
    unwrap = personListItems

-- | Optional query strings for people list API.
data PersonQuery = PersonQuery
    { personQueryEmail       :: Maybe Email             -- ^ Find person who has given email address.
    , personQueryDisplayName :: Maybe DisplayName       -- ^ Find person who has given display name.
    , personQueryOrgId       :: Maybe OrganizationId    -- ^ Find person who belongs to given organization.
    } deriving (Eq, Show)

-- | Default value of query strings for people list API.
defaultPersonQuery :: PersonQuery
defaultPersonQuery = PersonQuery Nothing Nothing Nothing

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

-- | 'TeamList' is decoded from response JSON of List Teams REST call.  It is list of 'Team'.
newtype TeamList = TeamList { teamListItems :: [Team] } deriving (Eq, Show)
$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLow 8, omitNothingFields = True } ''TeamList)
-- ^ 'TeamList' derives ToJSON and FromJSON via deriveJSON template haskell function.

instance SparkListItem Team where
    type ToList Team = TeamList
    unwrap = teamListItems

-- | 'CreateTeam' is encoded to request body JSON of Create a Team REST call.
newtype CreateTeam = CreateTeam { createTeamName :: TeamName } deriving (Eq, Show)
$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLow 10, omitNothingFields = True } ''CreateTeam)
-- ^ 'CreateTeam' derives ToJSON and FromJSON via deriveJSON template haskell function.

-- | 'UpdateTeam' is encoded to request body JSON of Update a Team REST call.
newtype UpdateTeam = UpdateTeam { updateTeamName :: TeamName } deriving (Eq, Show)
$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLow 10, omitNothingFields = True } ''UpdateTeam)
-- ^ 'UpdateTeam' derives ToJSON and FromJSON via deriveJSON template haskell function.


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

-- | 'TeamMembershipList' is decoded from response JSON of List Team Memberships REST call.  It is list of 'TeamMembership'.
newtype TeamMembershipList = TeamMembershipList { teamMembershipListItems :: [TeamMembership] } deriving (Eq, Show)
$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLow 18, omitNothingFields = True } ''TeamMembershipList)
-- ^ 'TeamMembershipList' derives ToJSON and FromJSON via deriveJSON template haskell function.

instance SparkListItem TeamMembership where
    type ToList TeamMembership = TeamMembershipList
    unwrap = teamMembershipListItems

-- | Optional query strings for team membership list API
newtype TeamMembershipQuery = TeamMembershipQuery
    { teamMembershipQueryTeamId :: Maybe TeamId -- ^ List membership only in given team.
    } deriving (Eq, Show)

-- | Default value of query strings for team membership list API.
defaultTeamMembershipQuery :: TeamMembershipQuery
defaultTeamMembershipQuery = TeamMembershipQuery Nothing

-- | 'CreateTeamMembership' is encoded to request body JSON of Create a Team Membership REST call.
data CreateTeamMembership = CreateTeamMembership
    { createTeamMembershipTeamId      :: TeamId         -- ^ Identifier of 'Team' which the user will be added to.
    , createTeamMembershipPersonId    :: Maybe PersonId -- ^ Identifier of 'Person' who will be added to the Team.
    , createTeamMembershipPersonEmail :: Maybe Email    -- ^ Email of the Person to be added.
    , createTeamMembershipIsModerator :: Maybe Bool     -- ^ The user becomes a moderator of the team if True.
    } deriving (Eq, Show)

$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLow 20, omitNothingFields = True } ''CreateTeamMembership)
-- ^ 'CreateTeamMembership' derives ToJSON and FromJSON via deriveJSON template haskell function.

-- | 'UpdateTeamMembership' is encoded to request body JSON of Update a Team Membership REST call.
newtype UpdateTeamMembership = UpdateTeamMembership { updateTeamMembershipIsModerator :: Bool } deriving (Eq, Show)
$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLow 20, omitNothingFields = True } ''UpdateTeamMembership)
-- ^ 'UpdateTeamMembership' derives ToJSON and FromJSON via deriveJSON template haskell function.


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

-- | 'RoomList' is decoded from response JSON of List Rooms REST call.  It is list of 'Room'.
newtype RoomList = RoomList { roomListItems :: [Room] } deriving (Eq, Show)
$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLow 8, omitNothingFields = True } ''RoomList)
-- ^ 'RoomList' derives ToJSON and FromJSON via deriveJSON template haskell function.

instance SparkListItem Room where
    type ToList Room = RoomList
    unwrap = roomListItems

-- | Optional query strings for room list API
data RoomQuery = RoomQuery
    { roomQueryTeamId   :: Maybe TeamId     -- ^ List rooms only in given team.
    , roomQueryRoomType :: Maybe RoomType   -- ^ List given type rooms only.
    } deriving (Eq, Show)

-- | Default value of query strings for room list API.
defaultRoomQuery :: RoomQuery
defaultRoomQuery = RoomQuery Nothing Nothing

-- | Sum type to ByteString converter for 'RoomType'.
roomTypeToQueryString :: RoomType -> ByteString
roomTypeToQueryString RoomTypeDirect = "direct"
roomTypeToQueryString RoomTypeGroup  = "group"

-- | 'CreateRoom' is encoded to request body JSON of Create a Room REST call.
data CreateRoom = CreateRoom
    { createRoomTitle  :: RoomTitle     -- ^ Title text of newly created Room.
    , createRoomTeamId :: Maybe TeamId  -- ^ Identifier of 'Team' which the Room will belong to.  If Nothing, the new Room will be standalone.
    } deriving (Eq, Show)

$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLow 10, omitNothingFields = True } ''CreateRoom)
-- ^ 'CreateRoom' derives ToJSON and FromJSON via deriveJSON template haskell function.

-- | 'UpdateRoom' is encoded to request body JSON of Update a Room REST call.
newtype UpdateRoom = UpdateRoom { updateRoomTitle :: RoomTitle } deriving (Eq, Show)
$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLow 10, omitNothingFields = True } ''UpdateRoom)
-- ^ 'UpdateRoom' derives ToJSON and FromJSON via deriveJSON template haskell function.


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

-- | 'MessageList' is decoded from response JSON of List Messages REST call.  It is list of 'Message'.
newtype MessageList = MessageList { messageListItems :: [Message] } deriving (Eq, Show)
$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLow 11, omitNothingFields = True } ''MessageList)
-- ^ 'MessageList' derives ToJSON and FromJSON via deriveJSON template haskell function.

instance SparkListItem Message where
    type ToList Message = MessageList
    unwrap = messageListItems

-- | Sum type for mentionedPeople query string.  It can be "me" or 'PersonId'.
data MentionedPeople = MentionedPeopleMe | MentionedPeople PersonId deriving (Eq, Show)

-- | Optional query strings for message list API
data MessageQuery = MessageQuery
    { messageQueryRoomId          :: RoomId                 -- ^ Mandatory parameter which room to search.
    , messageQueryMentionedPeople :: Maybe MentionedPeople  -- ^ List messages only mentioned to given person.
    , messageQueryBefore          :: Maybe Timestamp        -- ^ List messages posted before given timestamp.
    , messageQueryBeforeMessage   :: Maybe MessageId        -- ^ List messages posted before given message.
    } deriving (Eq, Show)

{-|
    Default value of query strings for message list API.
    Because 'RoomId' is mandatory, user have to supply it in order to get rest of defaults.
-}
defaultMessageQuery :: RoomId -> MessageQuery
defaultMessageQuery roomId = MessageQuery roomId Nothing Nothing Nothing

-- | Sum type to ByteString converter for mentionedPeople query string.
mentionedPeopleToQueryString :: MentionedPeople -> ByteString
mentionedPeopleToQueryString MentionedPeopleMe                     = "me"
mentionedPeopleToQueryString (MentionedPeople (PersonId personId)) = encodeUtf8 personId

-- | 'CreateMessage' is encoded to request body JSON of Create a Message REST call.
data CreateMessage = CreateMessage
    { createMessageRoomId        :: Maybe RoomId            -- ^ Identifier of the 'Room' the message will be posted to.
    , createMessageToPersonId    :: Maybe PersonId          -- ^ Identifier of the 'Person' to whom the direct message will be sent.
    , createMessageToPersonEmail :: Maybe Email             -- ^ Email of Person who receives the Message.
    , createMessageText          :: Maybe MessageText       -- ^ Message body in plain text.
    , createMessageMarkdown      :: Maybe MessageMarkdown   -- ^ Message body in markdown format.
    , createMessageFiles         :: Maybe [FileUrl]         -- ^ URLs of Attached files to the message.
    } deriving (Eq, Show)

$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLow 10, omitNothingFields = True } ''CreateMessage)
-- ^ 'CreateMessage' derives ToJSON and FromJSON via deriveJSON template haskell function.


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
    , membershipPersonOrdId       :: OrganizationId -- ^ Identifier of 'Organization' which the Person belongs to.
    , membershipIsModerator       :: Bool           -- ^ True if the Person is a moderator of the room.
    , membershipIsMonitor         :: Bool           -- ^ True if the Person is monitoring the Room.
    , membershipCreated           :: Timestamp      -- ^ Timestamp when the Membership was created.
    } deriving (Eq, Show)

$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLow 10, omitNothingFields = True } ''Membership)
-- ^ 'Membership' derives ToJSON and FromJSON via deriveJSON template haskell function.

-- | 'MembershipList' is decoded from response JSON of List Memberships REST call.  It is list of 'Membership'.
newtype MembershipList = MembershipList { membershipListItems :: [Membership] } deriving (Eq, Show)
$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLow 14, omitNothingFields = True } ''MembershipList)
-- ^ 'MembershipList' derives ToJSON and FromJSON via deriveJSON template haskell function.

instance SparkListItem Membership where
    type ToList Membership = MembershipList
    unwrap = membershipListItems

-- | Optional query strings for room membership list API
data MembershipQuery = MembershipQuery
    { membershipQueryRoomId      :: Maybe RoomId    -- ^ List membership only in given room.
    , membershipQueryPersonId    :: Maybe PersonId  -- ^ List membership related to given person of personId.
    , membershipQueryPersonEmail :: Maybe Email     -- ^ List membership related to given person of email.
    } deriving (Eq, Show)

-- | Default value of query strings for room membership list API.
defaultMembershipQuery :: MembershipQuery
defaultMembershipQuery = MembershipQuery Nothing Nothing Nothing

-- | 'CreateMembership' is encoded to request body JSON of Create a Membership REST call.
data CreateMembership = CreateMembership
    { createMembershipRoomId      :: RoomId         -- ^ Identifier of 'Room' which the Person will be added to.
    , createMembershipPersonId    :: Maybe PersonId -- ^ Identifier of 'Person' who will be added to the Room.
    , createMembershipPersonEmail :: Maybe Email    -- ^ Email of the Person to be added.
    , createMembershipIsModerator :: Maybe Bool     -- ^ The Person becomes a moderator of the Room if True.
    } deriving (Eq, Show)

$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLow 16, omitNothingFields = True } ''CreateMembership)
-- ^ 'CreateMembership' derives ToJSON and FromJSON via deriveJSON template haskell function.

-- | 'UpdateMembership' is encoded to request body JSON of Update a Membership REST call.
newtype UpdateMembership = UpdateMembership { updateMembershipIsModerator :: Bool } deriving (Eq, Show)
$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLow 16, omitNothingFields = True } ''UpdateMembership)
-- ^ 'UpdateMembership' derives ToJSON and FromJSON via deriveJSON template haskell function.


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

-- | 'OrganizationList' is decoded from response JSON of List Organizations REST call.  It is list of 'Organization'.
newtype OrganizationList = OrganizationList { organizationListItems :: [Organization] } deriving (Eq, Show)
$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLow 16, omitNothingFields = True } ''OrganizationList)
-- ^ 'OrganizationList' derives ToJSON and FromJSON via deriveJSON template haskell function.

instance SparkListItem Organization where
    type ToList Organization = OrganizationList
    unwrap = organizationListItems

-- | Display name of License
newtype LicenseDisplayName  = LicenseDisplayName Text deriving (Eq, Show, Generic, ToJSON, FromJSON)
-- | Counting number of granted or consumed License
newtype LicenseUnit         = LicenseUnit Text deriving (Eq, Show, Generic, ToJSON, FromJSON)

{-|
    'License' is allowance for features and services of Cisco Spark subscription.
    License is decoded from response JSON of Get License Details REST call.
    It is also element type of response of List Licenses call.
-}
data License = License
    { licenseId            :: LicenseId             -- ^ Identifier of the License.
    , licenseDisplayName   :: LicenseDisplayName    -- ^ Display name of the License.
    , licenseTotalUnits    :: LicenseUnit           -- ^ Number of granted License.
    , licenseConsumedUnits :: LicenseUnit           -- ^ Number of currently consumed License.
    } deriving (Eq, Show)

$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLow 7, omitNothingFields = True } ''License)
-- ^ 'License' derives ToJSON and FromJSON via deriveJSON template haskell function.

-- | 'LicenseList' is decoded from response JSON of List Licenses REST call.  It is list of 'License'.
newtype LicenseList = LicenseList { licenseListItems :: [License] } deriving (Eq, Show)
$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLow 11, omitNothingFields = True } ''LicenseList)
-- ^ 'LicenseList' derives ToJSON and FromJSON via deriveJSON template haskell function.

instance SparkListItem License where
    type ToList License = LicenseList
    unwrap = licenseListItems

-- | Optional query strings for license list API
newtype LicenseQuery = LicenseQuery
    { licenseQueryOrgId :: Maybe OrganizationId -- ^ List licenses only applicable to given organization.
    } deriving (Eq, Show)

-- | Default value of query strings for license list API.
defaultLicenseQuery :: LicenseQuery
defaultLicenseQuery = LicenseQuery Nothing

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

-- | 'RoleList' is decoded from response JSON of List Role REST call.  It is list of 'Role'.
newtype RoleList = RoleList { roleListItems :: [Role] } deriving (Eq, Show)
$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLow 8, omitNothingFields = True } ''RoleList)
-- ^ 'RoleList' derives ToJSON and FromJSON via deriveJSON template haskell function.

instance SparkListItem Role where
    type ToList Role = RoleList
    unwrap = roleListItems

