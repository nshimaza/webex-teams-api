{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Conduit
import           Data.ByteString.Char8 as BC8 (pack)
import           Data.Default          (def)
import           Data.Semigroup        ((<>))
import           Data.Text             as T (pack)
import           Network.HTTP.Simple   (getResponseBody)
import           Options.Applicative
import           System.Environment    (lookupEnv)
import           System.IO             (hPutStrLn, stderr)

import           Network.CiscoSpark

data Command
    = TeamListCommand Int
    | RoomListCommand Int RoomFilter
    | PersonListCommand Int PersonFilter
    | PersonDetailCommand PersonId
    | RoomDetailCommand RoomId
    | MembershipListCommand Int MembershipFilter
    | MembershipDetailCommand MembershipId
    | MessageListCommand Int MessageFilter
    | MessageDetailCommand MessageId
    | TeamDetailCommand TeamId
    | TeamMembershipListCommand Int TeamMembershipFilter
    | TeamMembershipDetailCommand TeamMembershipId
    deriving (Show)


{-
    Common command line option parsers
-}
countParser :: Int -> Parser Int
countParser defaultCount = option auto
    (  long    "count"
    <> short   'c'
    <> metavar "MAX_ITEMS"
    <> value   (if defaultCount == def then maxBound else defaultCount)
    <> help "Maximum number of items to print"
    )

emailOptParser :: Parser Email
emailOptParser = Email . T.pack <$> strOption
    (  long     "email"
    <> short    'e'
    <> metavar  "EMAIL_ADDRESS"
    <> help     "An email address to filter result"
    )

teamIdOptParser :: Parser TeamId
teamIdOptParser = TeamId . T.pack <$> strOption
    (  long     "team"
    <> short    't'
    <> metavar "TEAM_ID"
    <> help    "Identifier of a team to filter result"
    )

{-
    Person specific command line option parsers
-}
personIdParser :: Parser PersonId
personIdParser = PersonId . T.pack <$> strArgument
    (  metavar "PERSON_ID"
    <> help    "Identifier of a person"
    )

displayNameParser :: Parser DisplayName
displayNameParser = DisplayName . T.pack <$> strOption
    (  long     "name"
    <> short    'n'
    <> metavar  "DISPLAY_NAME"
    <> help     "Display name of the person to look up"
    )

organizationIdOptParser :: Parser OrganizationId
organizationIdOptParser = OrganizationId . T.pack <$> strOption
    (  long     "org"
    <> short    'o'
    <> metavar "ORGANIZATION_ID"
    <> help    "Identifier of a organization to be searched"
    )

personFilterParser :: Parser PersonFilter
personFilterParser = PersonFilter <$> optional emailOptParser <*> optional displayNameParser <*> optional organizationIdOptParser

personListOptParser :: Parser Command
personListOptParser = PersonListCommand <$> countParser def <*> personFilterParser

personDetailOptParser :: Parser Command
personDetailOptParser = PersonDetailCommand <$> personIdParser

{-
    Room specific command line option parsers
-}
roomIdParser :: Parser RoomId
roomIdParser = RoomId . T.pack <$> strArgument
    (  metavar "ROOM_ID"
    <> help    "Identifier of a room")

roomTypeParser :: Parser (Maybe RoomType)
roomTypeParser
    =   flag Nothing (Just RoomTypeDirect)
            (  long  "direct"
            <> short 'd'
            <> help  "Filter only one-to-one space")
    <|> flag Nothing (Just RoomTypeGroup)
            (  long  "group"
            <> short 'g'
            <> help  "Filter only group space")

roomSortByParser :: Parser (Maybe RoomFilterSortBy)
roomSortByParser
    =   flag Nothing (Just RoomFilterSortById)
            (  long "sort-by-id"
            <> help "Sort by room ID")
    <|> flag Nothing (Just RoomFilterSortByLastActivity)
            (  long "sort-by-last-activity"
            <> help "Sort by most recent activity")
    <|> flag Nothing (Just RoomFilterSortByCreated)
            (  long "sort-by-created"
            <> help "Sort by most recentlly created")

roomListOptParser :: Parser Command
roomListOptParser = RoomListCommand <$> countParser def
                                    <*> (RoomFilter <$> optional teamIdOptParser
                                                    <*> roomTypeParser
                                                    <*> roomSortByParser)

roomDetailOptParser :: Parser Command
roomDetailOptParser = RoomDetailCommand <$> roomIdParser

{-
    Membership specific command line option parsers
-}
membershipIdParser :: Parser MembershipId
membershipIdParser = MembershipId . T.pack <$> strArgument
    (  metavar "MEMBERSHIP_ID"
    <> help    "Identifier of a membership"
    )

roomIdOptParser :: Parser RoomId
roomIdOptParser = RoomId . T.pack <$> strOption
    (  long     "room"
    <> short    'r'
    <> metavar "ROOM_ID"
    <> help    "Identifier of a room to be searched"
    )

personIdOptParser :: Parser PersonId
personIdOptParser = PersonId . T.pack <$> strOption
    (  long     "person"
    <> short    'p'
    <> metavar "PERSON_ID"
    <> help    "Identifier of a person to filter result"
    )

membershipListOptParser :: Parser Command
membershipListOptParser = MembershipListCommand <$> countParser def
                                                <*> (MembershipFilter <$> optional roomIdOptParser
                                                                      <*> optional personIdOptParser
                                                                      <*> optional emailOptParser)

membershipDetailOptParser :: Parser Command
membershipDetailOptParser = MembershipDetailCommand <$> membershipIdParser

{-
    Message specific command line option parsers
-}
messageIdParser :: Parser MessageId
messageIdParser = MessageId . T.pack <$> strArgument
    (  metavar "MESSAGE_ID"
    <> help    "Identifier of a message"
    )

mentionedPeopleParser :: Parser (Maybe MentionedPeople)
mentionedPeopleParser
    =   flag Nothing (Just MentionedPeopleMe)
            (  long "me"
            <> help "List messages where the caller is mentioned"
            )
    <|> optional (MentionedPeople . PersonId . T.pack <$> strOption
                    (   long    "mentioned-person"
                    <>  short   'p'
                    <>  metavar "PERSON_ID"
                    <>  help    "List messages where the specified persionId is mentioned"
                    )
                 )

beforeOptParser :: Parser Timestamp
beforeOptParser = Timestamp . T.pack <$> strOption
    (  long     "before"
    <> short    'b'
    <> metavar  "TIMESTAMP"
    <> help     "List messages sent before a date and time, in ISO8601 format"
    )

beforeMessageOptParser :: Parser MessageId
beforeMessageOptParser = MessageId . T.pack <$> strOption
    (  long     "before-message"
    <> short    'm'
    <> metavar  "MESSAGE_ID"
    <> help     "List messages sent before a message, by ID"
    )

messageListOptParser :: Parser Command
messageListOptParser = MessageListCommand <$> countParser 10
                                          <*> (MessageFilter <$> roomIdParser
                                                             <*> mentionedPeopleParser
                                                             <*> optional beforeOptParser
                                                             <*> optional beforeMessageOptParser
                                                             )

messageDetailOptParser :: Parser Command
messageDetailOptParser = MessageDetailCommand <$> messageIdParser

{-
    Team specific command line option parsers
-}
teamIdParser :: Parser TeamId
teamIdParser = TeamId . T.pack <$> strArgument
    (  metavar "TEAM_ID"
    <> help    "Identifier of a team"
    )

teamListOptParser :: Parser Command
teamListOptParser = TeamListCommand <$> countParser def

teamDetailOptParser :: Parser Command
teamDetailOptParser = TeamDetailCommand <$> teamIdParser

{-
    Team membership specific command line option parsers
-}
teamMembershipIdParser :: Parser TeamMembershipId
teamMembershipIdParser = TeamMembershipId . T.pack <$> strArgument
    (  metavar "TEAM_MEMBERSHIP_ID"
    <> help    "Identifier of a team membership"
    )

teamMembershipListOptParser :: Parser Command
teamMembershipListOptParser = TeamMembershipListCommand <$> countParser def
                                                        <*> (TeamMembershipFilter <$> teamIdParser)

teamMembershipDetailOptParser :: Parser Command
teamMembershipDetailOptParser = TeamMembershipDetailCommand <$> teamMembershipIdParser

{-
    Top level parsers
-}
commandSubParser :: Parser Command
commandSubParser = hsubparser
    (  command "team-list" (info teamListOptParser (progDesc "List belonging teams"))
    <> command "team-detail" (info teamDetailOptParser (progDesc "Get detail for a team by ID"))
    <> command "person-list" (info personListOptParser (progDesc "List people"))
    <> command "person-detail" (info personDetailOptParser (progDesc "Get detail for a person by ID"))
    <> command "room-list" (info roomListOptParser (progDesc "List belonging spaces"))
    <> command "room-detail" (info roomDetailOptParser (progDesc "Get detail for a team by ID"))
    <> command "membership-list" (info membershipListOptParser (progDesc "List memberships of authenticated user"))
    <> command "membership-detail" (info membershipDetailOptParser (progDesc "Get detail for a membership by ID"))
    <> command "message-list" (info messageListOptParser (progDesc "List messages in a room"))
    <> command "message-detail" (info messageDetailOptParser (progDesc "Get detail for a message by ID"))
    <> command "team-membership-list" (info teamMembershipListOptParser (progDesc "List team memberships of authenticated user"))
    <> command "team-membership-detail" (info teamMembershipDetailOptParser (progDesc "Get detail for a team membership by ID"))
    )

programOptions :: ParserInfo Command
programOptions = info (commandSubParser <**> helper)
    (  fullDesc
    <> progDesc "Sample porgram demonstrating how to use cisco-spark-api"
    <> header   "cisco-spark-api-exe -- Sample porgram demonstrating how to use cisco-spark-api"
    )

run :: Authorization -> Command -> IO ()
run auth (PersonListCommand count filter) =
    runConduit $ streamEntityWithFilter auth def filter .| takeC count .| mapM_C print

run auth (RoomListCommand count filter) =
    runConduit $ streamEntityWithFilter auth def filter .| takeC count .| mapM_C print

run auth (MembershipListCommand count filter) =
    runConduit $ streamEntityWithFilter auth def filter .| takeC count .| mapM_C print

run auth (MessageListCommand count filter) =
    runConduit $ streamEntityWithFilter auth def filter .| takeC count .| mapM_C print

run auth (TeamMembershipListCommand count filter) =
    runConduit $ streamEntityWithFilter auth def filter .| takeC count .| mapM_C print

run auth (TeamListCommand count) =
    runConduit $ streamTeamList auth def .| takeC count .| mapM_C print

run auth (PersonDetailCommand personId) =
    getDetail auth def personId >>= print . getResponseBody

run auth (RoomDetailCommand roomId) =
    getDetail auth def roomId >>= print . getResponseBody

run auth (MembershipDetailCommand membershipId) =
    getDetail auth def membershipId >>= print . getResponseBody

run auth (MessageDetailCommand messageId) =
    getDetail auth def messageId >>= print . getResponseBody

run auth (TeamDetailCommand teamId) =
    getDetail auth def teamId >>= print . getResponseBody

run auth (TeamMembershipDetailCommand teamMembershipId) =
    getDetail auth def teamMembershipId >>= print . getResponseBody


main :: IO ()
main = do
    lookupEnv "SPARK_AUTH" >>= runIfEnvFound
      where
        runIfEnvFound Nothing   = hPutStrLn stderr "Missing SPARK_AUTH.  Set Spark authorization string to SPARK_AUTH environment variable."
        runIfEnvFound (Just s)  = do
            opts <- execParser programOptions
            print opts
            run (Authorization $ BC8.pack s) opts
