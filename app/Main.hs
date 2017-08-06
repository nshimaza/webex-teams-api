{-# LANGUAGE OverloadedStrings #-}

module Main where

import Conduit
import           Data.ByteString.Char8 as BC8 (pack)
import           Data.Semigroup        ((<>))
import           Options.Applicative

import           Network.CiscoSpark


auth :: Parser Authorization
auth = Authorization . BC8.pack <$> strOption
    (  long "auth"
    <> short 'a'
    <> metavar "AUTHORIZATION"
    <> help "Authorization token string passed via HTTP Authorization header.")

opts = info (auth <**> helper)
    (  fullDesc
    <> progDesc "Sample porgram demonstrating how to use cisco-spark-api."
    <> header "cisco-spark-api-exe -a auth"
    )

main :: IO ()
main = do
    auth <- execParser opts
    print auth
    runConduit $ streamTeamList auth ciscoSparkBaseRequest .| mapM_C print
