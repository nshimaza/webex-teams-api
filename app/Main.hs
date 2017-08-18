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
    | RoomListCommand Authorization Int RoomFilter
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

roomFilterParser :: Parser RoomFilter
roomFilterParser = RoomFilter <$> maybeTeamIdParser <*> roomTypeParser <*> roomSortByParser

roomListOptParser :: Parser Command
roomListOptParser = RoomListCommand <$> authParser <*> countParser <*> roomFilterParser

roomDetailOptParser :: Parser Command
roomDetailOptParser = RoomDetailCommand <$> authParser <*> roomIdParser

{-
    Top level parsers
-}
commandSubParser :: Parser Command
commandSubParser = hsubparser
    (  command "team-list" (info teamListOptParser (progDesc "List belonging teams"))
    <> command "team-detail" (info teamDetailOptParser (progDesc "Get detail for a team by ID"))
    <> command "room-list" (info roomListOptParser (progDesc "List belonging spaces"))
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

run (RoomListCommand auth count filter) =
    runConduit $ streamEntityWithFilter auth def filter .| takeC count .| mapM_C print

run (TeamDetailCommand auth teamId) =
    getDetail auth def teamId >>= print . getResponseBody

run (RoomDetailCommand auth roomId) =
    getDetail auth def roomId >>= print . getResponseBody

main :: IO ()
main = do
    opts <- execParser programOptions
    print opts
    run opts
