{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Conduit
import           Data.ByteString.Char8 as BC8 (pack)
import           Data.Default          (def)
import           Data.Semigroup        ((<>))
import           Data.Text             as T (pack)
import           Network.HTTP.Simple   (getResponseBody)
import           Options.Applicative

import           Network.CiscoSpark

data Command
    = TeamListCommand Authorization Int
    | RoomListCommand Authorization Int RoomQuery
    | TeamDetailCommand Authorization TeamId
    | RoomDetailCommand Authorization RoomId
    deriving (Show)


data Options = Options
    { optAuth  :: Authorization
    , optCount :: Int
    } deriving (Show)

{-
    Common command line option parsers
-}
authParser :: Parser Authorization
authParser = Authorization . BC8.pack <$> strOption
    (  long    "auth"
    <> short   'a'
    <> metavar "AUTHORIZATION"
    <> help    "Authorization token string passed via HTTP Authorization header")

countParser :: Parser Int
countParser = option auto
    (  long    "count"
    <> short   'c'
    <> metavar "MAX_ITEMS"
    <> value   (maxBound :: Int)
    <> help "Maximum number of items to print"
    )

{-
    Team specific command line option parsers
-}
teamIdParser :: Parser TeamId
teamIdParser = TeamId . T.pack <$> strOption
    (  long    "id"
    <> short   'i'
    <> metavar "TEAM_ID"
    <> help    "Identifier of a team"
    )

maybeTeamIdParser :: Parser (Maybe TeamId)
maybeTeamIdParser = optional teamIdParser

teamListOptParser :: Parser Command
teamListOptParser = TeamListCommand <$> authParser <*> countParser

teamDetailOptParser :: Parser Command
teamDetailOptParser = TeamDetailCommand <$> authParser <*> teamIdParser

{-
    Room specific command line option parsers
-}
roomIdParser :: Parser RoomId
roomIdParser = RoomId . T.pack <$> strOption
    (  long    "id"
    <> short   'i'
    <> metavar "ROOM_ID"
    <> help    "Identifier of a room")

roomTypeParser :: Parser (Maybe RoomType)
roomTypeParser
    =   flag Nothing (Just RoomTypeDirect)
            (  long  "direct"
            <> short 'd'
            <> help  "Query only one-to-one space")
    <|> flag Nothing (Just RoomTypeGroup)
            (  long  "group"
            <> short 'g'
            <> help  "Query only group space")

roomSortByParser :: Parser (Maybe RoomQuerySortBy)
roomSortByParser
    =   flag Nothing (Just RoomQuerySortById)
            (  long "sort-by-id"
            <> help "Sort by room ID")
    <|> flag Nothing (Just RoomQuerySortByLastActivity)
            (  long "sort-by-last-activity"
            <> help "Sort by most recent activity")
    <|> flag Nothing (Just RoomQuerySortByCreated)
            (  long "sort-by-created"
            <> help "Sort by most recentlly created")

roomQueryParser :: Parser RoomQuery
roomQueryParser = RoomQuery <$> maybeTeamIdParser <*> roomTypeParser <*> roomSortByParser

roomListOptParser :: Parser Command
roomListOptParser = RoomListCommand <$> authParser <*> countParser <*> roomQueryParser

roomDetailOptParser :: Parser Command
roomDetailOptParser = RoomDetailCommand <$> authParser <*> roomIdParser

{-
    Top level parsers
-}
commandSubParser :: Parser Command
commandSubParser = hsubparser
    (  command "team-list" (info teamListOptParser (progDesc "Query belonging teams"))
    <> command "team-detail" (info teamDetailOptParser (progDesc "Get detail for a team by ID"))
    <> command "room-list" (info roomListOptParser (progDesc "Query belonging spaces"))
    <> command "room-detail" (info roomDetailOptParser (progDesc "Get detail for a team by ID"))
    )

programOptions :: ParserInfo Command
programOptions = info (commandSubParser <**> helper)
    (  fullDesc
    <> progDesc "Sample porgram demonstrating how to use cisco-spark-api"
    <> header   "cisco-spark-api-exe -- Sample porgram demonstrating how to use cisco-spark-api"
    )

run :: Command -> IO ()
run (TeamListCommand auth count) =
    runConduit $ streamTeamList auth def .| takeC count .| mapM_C print

run (RoomListCommand auth count query) =
    runConduit $ streamRoomList auth def query .| takeC count .| mapM_C print

run (TeamDetailCommand auth teamId) =
    getDetail def auth teamId >>= print . getResponseBody

run (RoomDetailCommand auth roomId) =
    getDetail def auth roomId >>= print . getResponseBody

main :: IO ()
main = do
    opts <- execParser programOptions
    print opts
    run opts
