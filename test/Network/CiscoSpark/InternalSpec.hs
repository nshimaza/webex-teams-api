{-# LANGUAGE OverloadedStrings #-}

module Network.CiscoSpark.InternalSpec where

import           Data.Attoparsec.ByteString
import           Data.ByteString            (pack)
import           Data.Either                (isLeft)

import           Test.Hspec

import           Network.CiscoSpark.Internal


spec :: Spec
spec = do
    describe "Parser elements" $ do
        it "decodes DQUOTE" $ do
            parseOnly dQuote "\"" `shouldBe` Right 0x22

        it "decodes quoted-pair" $ do
            parseOnly quotedPair "\\\\" `shouldBe` Right 0x5c
            parseOnly quotedPair "\\\"" `shouldBe` Right 0x22
            parseOnly quotedPair "no quoted pair" `shouldSatisfy` isLeft

        it "decodes quoted-string of '\"next\"' as 'next'" $ do
            parseOnly quotedString "\"next\"" `shouldBe` Right "next"

        it "decodes rel parameter as a token" $ do
            parseOnly token "rel" `shouldBe` Right "rel"

        it "decodes rel=\"next\" as LinkParam and its value" $ do
            parseOnly param "; rel=\"next\"" `shouldBe` Right (Rel, "next")

        it "decodes rel=next as LinkParam and its value" $ do
            parseOnly param "; rel=next" `shouldBe` Right (Rel, "next")

        it "decodes entire link header value" $ do
            parseOnly linkHeader "<https://api.ciscospark.com/v1/people?link=to&next=page>; rel=\"next\""
                `shouldBe` Right (LinkHeader "https://api.ciscospark.com/v1/people?link=to&next=page" [ (Rel, "next") ])

    describe "HTTP Link header detector" $ do
        it "returns empty list from empty input" $ do
            extractNextUrl [] `shouldBe` []

        it "extracts a URL from a header value when the header has rel=next param" $ do
            extractNextUrl ["<https://api.ciscospark.com/v1/people?link=to&next=page>; rel=\"next\""]
                `shouldBe` ["https://api.ciscospark.com/v1/people?link=to&next=page"]

        it "ignores header which has no rel=next param" $ do
            extractNextUrl ["<http://hosta>; norel=next", "<http://hostb>; rel=next"] `shouldBe` ["http://hostb"]

        it "ignores invalid header" $ do
            extractNextUrl ["invalid header", "<http://valid>; rel=next"] `shouldBe` ["http://valid"]

