{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.CiscoSparkSpec where

import           Conduit
import           Control.Concurrent.MVar      (MVar, newEmptyMVar, putMVar,
                                               takeMVar)
import           Control.Monad                (void)
import           Data.Aeson                   (encode)
import           Data.Attoparsec.ByteString   (parseOnly)
import           Data.ByteString.Char8        as C8 (unpack)
import           Data.ByteString.Lazy         as L (ByteString)
import           Data.Default                 (def)
import           Data.List                    (sort)
import           Data.Maybe                   (fromJust)
import           Data.Monoid                  ((<>))
import           Data.Text                    (pack)

import           Network.HTTP.Simple          as C

import           Network.HTTP.Types           (Header, status200)
import           Network.Wai
import           Network.Wai.Handler.Warp     (Settings, defaultSettings,
                                               runSettings, setBeforeMainLoop)

import           Test.Hspec

import           Control.Concurrent.Hierarchy (ThreadMap, killThreadHierarchy,
                                               newChild, newThreadMap)
import           Network.CiscoSpark
import           Network.CiscoSpark.Internal  (LinkHeader (..), LinkParam (..),
                                               getNextUrl, linkHeader)


import           Data.Typeable                (typeOf)


newtype MockServer = MockServer ThreadMap

mockBaseRequest :: CiscoSparkRequest
mockBaseRequest
    = CiscoSparkRequest
    $ C.addRequestHeader "Content-Type" "application/json; charset=utf-8"
    $ C.setRequestPort 3000
    $ C.setRequestHost "localhost"
    $ C.setRequestSecure False
    $ C.defaultRequest

dummyAuth :: Authorization
dummyAuth = Authorization "dummyAuth"

extractRight :: Show err => Either err r -> r
extractRight (Right r)  = r
extractRight (Left err) = error $ show err

startMockServer :: Application -> IO MockServer
startMockServer app = do
    readyToConnect <- newEmptyMVar
    rootThreadMap <- newThreadMap
    let set = setBeforeMainLoop (putMVar readyToConnect ()) defaultSettings
    newChild rootThreadMap $ \_ -> runSettings set app
    takeMVar readyToConnect
    return $ MockServer rootThreadMap

stopMockServer :: MockServer -> IO ()
stopMockServer (MockServer rootThreadMap) = killThreadHierarchy rootThreadMap

helloApp :: Application
helloApp _ respond = respond $ responseLBS status200 [] "hello, world"

contentType :: Header
contentType = ("Content-Type", "application/json; charset=utf-8")

simpleApp :: L.ByteString -> Application
simpleApp res _ respond = respond $ responseLBS status200 [contentType] res

paginationApp :: [L.ByteString] -> Application
paginationApp ress req respond = do
    let (cTypes, body) = dispatch $ rawPathInfo req
    respond $ responseLBS status200 cTypes body
      where
        dispatch "/1"   = ([contentType, ("Link", "<http://localhost:3000/2>; rel=\"next\"")], ress !! 1)
        dispatch "/2"   = ([contentType, ("Link", "<http://localhost:3000/3>; rel=\"next\"")], ress !! 2)
        dispatch "/3"   = ([contentType], ress !! 3)
        dispatch _      = ([contentType, ("Link", "<http://localhost:3000/1>; rel=\"next\"")], ress !! 0)

teamGen :: String -> Team
teamGen i = Team { teamId           = TeamId . pack $ "teamId" <> i
                 , teamName         = TeamName . pack $ "teamName" <> i
                 , teamCreatorId    = PersonId . pack $ "teamCreatorId" <> i
                 , teamCreated      = Timestamp . pack $ "teamCreated" <> i
                 }

teamList :: String -> [Team]
teamList j = [ teamGen $ j <> show i | i <- [1..3] ]

teamListList :: [[Team]]
teamListList = [ teamList [c] | c <- ['a'..'d'] ]

spec :: Spec
spec = do
    describe "Mock Applications" $ do
        it "simple mock app returns list of team" $ do
            receivedReqMVar <- newEmptyMVar
            let (CiscoSparkRequest base) = mockBaseRequest
                req = setRequestPath "/v1/teams"
                    $ setRequestMethod "GET"
                    $ base
                testData = TeamList $ teamList ['Z']

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp (encode testData) req respond

            res <- getResponseBody <$> httpJSON req
            res `shouldBe` testData

            receivedReq <- takeMVar receivedReqMVar
            requestMethod receivedReq `shouldBe` "GET"
            rawPathInfo receivedReq `shouldBe` "/v1/teams"
            (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"

            stopMockServer svr

        it "pagenation mock app returns list of team and Link header" $ do
            receivedReqMVar <- newEmptyMVar
            let (CiscoSparkRequest base) = mockBaseRequest
                req = setRequestPath "/v1/teams"
                    $ setRequestMethod "GET"
                    $ base
                testData = encode . TeamList <$> teamListList

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                paginationApp testData req respond

            res1 <- httpJSON req
            getResponseBody res1 `shouldBe` TeamList (teamListList !! 0)

            receivedReq <- takeMVar receivedReqMVar
            rawPathInfo receivedReq `shouldBe` "/v1/teams"

            let path = getNextUrl res1
            path `shouldBe` Just "http://localhost:3000/1"

            req2 <- parseRequest $ "GET " <> C8.unpack (fromJust path)
            res2 <- httpJSON req2
            getResponseBody res2 `shouldBe` TeamList (teamListList !! 1)

            stopMockServer svr

    describe "People" $ do
        let personJson1 = "{\
                          \  \"id\" : \"Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mNWIzNjE4Ny1jOGRkLTQ3MjctOGIyZi1mOWM0NDdmMjkwNDY\",\
                          \  \"emails\" : [ \"johnny.chang@foomail.com\", \"jchang@barmail.com\" ],\
                          \  \"displayName\" : \"John Andersen\",\
                          \  \"firstName\" : \"John\",\
                          \  \"lastName\" : \"Andersen\",\
                          \  \"avatar\" : \"https://1efa7a94ed21783e352-c62266528714497a17239ececf39e9e2.ssl.cf1.rackcdn.com/V1~54c844c89e678e5a7b16a306bc2897b9~wx29yGtlTpilEFlYzqPKag==~1600\",\
                          \  \"orgId\" : \"Y2lzY29zcGFyazovL3VzL09SR0FOSVpBVElPTi85NmFiYzJhYS0zZGNjLTExZTUtYTE1Mi1mZTM0ODE5Y2RjOWE\",\
                          \  \"roles\" : [ \"Y2lzY29zcGFyazovL3VzL1JPT00vOGNkYzQwYzQtZjA5ZS0zY2JhLThjMjYtZGQwZTcwYWRlY2Iy\", \"Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mMDZkNzFhNS0wODMzLTRmYTUtYTcyYS1jYzg5YjI1ZWVlMmX\" ],\
                          \  \"licenses\" : [ \"Y2lzY29zcGFyazovL3VzL1JPT00vOGNkYzQwYzQtZjA5ZS0zY2JhLThjMjYtZGQwZTcwYWRlY2Iy\", \"Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mMDZkNzFhNS0wODMzLTRmYTUtYTcyYS1jYzg5YjI1ZWVlMmX\" ],\
                          \  \"created\" : \"2015-10-18T14:26:16.000Z\",\
                          \  \"timezone\" : \"America/Denver\",\
                          \  \"lastActivity\" : \"2015-10-18T14:26:16.028Z\",\
                          \  \"status\" : \"active\",\
                          \  \"invitePending\" : false,\
                          \  \"loginEnabled\" : true\
                          \}"
            person1 = Person { personId            = PersonId "Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mNWIzNjE4Ny1jOGRkLTQ3MjctOGIyZi1mOWM0NDdmMjkwNDY"
                             , personEmails        = [Email "johnny.chang@foomail.com", Email "jchang@barmail.com"]
                             , personDisplayName   = DisplayName "John Andersen"
                             , personNickName      = Nothing
                             , personFirstName     = Just (FirstName "John")
                             , personLastName      = Just (LastName "Andersen")
                             , personAvatar        = Just (AvatarUrl "https://1efa7a94ed21783e352-c62266528714497a17239ececf39e9e2.ssl.cf1.rackcdn.com/V1~54c844c89e678e5a7b16a306bc2897b9~wx29yGtlTpilEFlYzqPKag==~1600")
                             , personOrgId         = OrganizationId "Y2lzY29zcGFyazovL3VzL09SR0FOSVpBVElPTi85NmFiYzJhYS0zZGNjLTExZTUtYTE1Mi1mZTM0ODE5Y2RjOWE"
                             , personRoles         = Just [ RoleId "Y2lzY29zcGFyazovL3VzL1JPT00vOGNkYzQwYzQtZjA5ZS0zY2JhLThjMjYtZGQwZTcwYWRlY2Iy"
                                                          , RoleId "Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mMDZkNzFhNS0wODMzLTRmYTUtYTcyYS1jYzg5YjI1ZWVlMmX"]
                             , personLicenses      = Just [ LicenseId "Y2lzY29zcGFyazovL3VzL1JPT00vOGNkYzQwYzQtZjA5ZS0zY2JhLThjMjYtZGQwZTcwYWRlY2Iy"
                                                          , LicenseId "Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mMDZkNzFhNS0wODMzLTRmYTUtYTcyYS1jYzg5YjI1ZWVlMmX"]
                             , personCreated       = Timestamp "2015-10-18T14:26:16.000Z"
                             , personTimezone      = Just (Timezone "America/Denver")
                             , personLastActivity  = Just (Timestamp "2015-10-18T14:26:16.028Z")
                             , personStatus        = Just PersonStatusActive
                             , personInvitePending = Just False
                             , personLoginEnabled  = Just True
                             , personType          = Nothing
                             }
            personGen i = Person { personId            = PersonId . pack $ "PersonId" <> i
                                 , personEmails        = [Email . pack $ "email" <> i <> "@foomail.com", Email . pack $ "email" <> i <> "@barmail.com"]
                                 , personDisplayName   = DisplayName . pack $ "John Andersen" <> i
                                 , personNickName      = Nothing
                                 , personFirstName     = Just (FirstName . pack $ "John" <> i)
                                 , personLastName      = Just (LastName . pack $ "Andersen" <> i)
                                 , personAvatar        = Just (AvatarUrl . pack $ "https://AvatarUrl" <> i)
                                 , personOrgId         = OrganizationId . pack $ "OrganizationId" <> i
                                 , personRoles         = Just [ RoleId . pack $ "RoleIdA" <> i
                                                              , RoleId . pack $ "RoleIdB" <> i]
                                 , personLicenses      = Just [ LicenseId . pack $ "LicenseIdX" <> i
                                                              , LicenseId . pack $ "LicenseIdY" <> i]
                                 , personCreated       = Timestamp . pack $ "Created" <> i
                                 , personTimezone      = Just (Timezone . pack $ "Timezone" <> i)
                                 , personLastActivity  = Just (Timestamp . pack $ "LastActivity" <> i)
                                 , personStatus        = Just PersonStatusActive
                                 , personInvitePending = Just False
                                 , personLoginEnabled  = Just True
                                 , personType          = Nothing
                                 }
            personList j = [ personGen $ j <> show i | i <- [1..3] ]
            personListList = [ personList [c] | c <- ['a'..'d'] ]

        it "streamPersonList streams Team" $ do
            let testData = personList ['Z']
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp (encode (PersonList testData)) req respond

            res <- runConduit $ streamPersonList dummyAuth mockBaseRequest def .| sinkList
            res `shouldBe` testData

            receivedReq <- takeMVar receivedReqMVar
            rawPathInfo receivedReq `shouldBe` "/v1/people"
            queryString receivedReq `shouldBe` []

            stopMockServer svr

        it "streamPersonList passes query strings build from PersonQuery to server" $ do
            let testData = personList ['Z']
                personQuery = PersonQuery (Just $ Email "person@query.com")
                                          (Just $ DisplayName "DisplayNameQuery")
                                          (Just $ OrganizationId "OrgIdQuery")

            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp (encode (PersonList testData)) req respond

            res <- runConduit $ streamPersonList dummyAuth mockBaseRequest personQuery .| sinkList
            res `shouldBe` testData

            receivedReq <- takeMVar receivedReqMVar
            rawPathInfo receivedReq `shouldBe` "/v1/people"
            (sort . queryString) receivedReq `shouldBe` sort [ ("orgId", Just "OrgIdQuery")
                                                             , ("displayName", Just "DisplayNameQuery")
                                                             , ("email", Just "person@query.com") ]

            stopMockServer svr

        it "streamPersonList streams Team with automatic pagination" $ do
            svr <- startMockServer . paginationApp $ encode . PersonList <$> personListList

            res <- runConduit $ streamPersonList dummyAuth mockBaseRequest def .| sinkList
            res `shouldBe` concat personListList

            stopMockServer svr

        it "getPersonDetail returns a Person" $ do
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                respond $ responseLBS status200 [] personJson1

            resPerson <- getResponseBody <$> getPersonDetail mockBaseRequest dummyAuth (PersonId "testPersonId")
            resPerson `shouldBe` person1

            receivedReq <- takeMVar receivedReqMVar
            requestMethod receivedReq `shouldBe` "GET"
            rawPathInfo receivedReq `shouldBe` "/v1/people/testPersonId"
            (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
            (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"

            stopMockServer svr

        it "getPersonDetailEither returns a (Right Person)" $ do
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                respond $ responseLBS status200 [] personJson1

            (Right resPerson) <- getResponseBody <$> getPersonDetailEither mockBaseRequest dummyAuth (PersonId "testPersonId")
            resPerson `shouldBe` person1

            stopMockServer svr

    describe "Team" $ do
        let teamJson = "{\
                       \  \"id\" : \"Y2lzY29zcGFyazovL3VzL1RFQU0vMTNlMThmNDAtNDJmYy0xMWU2LWE5ZDgtMjExYTBkYzc5NzY5\",\
                       \  \"name\" : \"Build Squad\",\
                       \  \"creatorId\" : \"Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mNWIzNjE4Ny1jOGRkLTQ3MjctOGIyZi1mOWM0NDdmMjkwNDY\",\
                       \  \"created\" : \"2015-10-18T14:26:16+00:00\"\
                       \}"
            team = Team { teamId        = TeamId "Y2lzY29zcGFyazovL3VzL1RFQU0vMTNlMThmNDAtNDJmYy0xMWU2LWE5ZDgtMjExYTBkYzc5NzY5"
                        , teamName      = TeamName "Build Squad"
                        , teamCreatorId = PersonId "Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mNWIzNjE4Ny1jOGRkLTQ3MjctOGIyZi1mOWM0NDdmMjkwNDY"
                        , teamCreated   = Timestamp "2015-10-18T14:26:16+00:00"
                        }

        it "streamTeamList streams Team" $ do
            let testData = teamList ['Z']
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp (encode (TeamList testData)) req respond

            res <- runConduit $ streamTeamList dummyAuth mockBaseRequest .| sinkList
            res `shouldBe` testData
            path <- rawPathInfo <$> takeMVar receivedReqMVar
            path `shouldBe` "/v1/teams"

            stopMockServer svr

        it "streamTeamList streams Team with automatic pagination" $ do
            svr <- startMockServer . paginationApp $ encode . TeamList <$> teamListList

            res <- runConduit $ streamTeamList dummyAuth mockBaseRequest .| sinkList
            res `shouldBe` concat teamListList

            stopMockServer svr

        it "getTeamDetail returns a Team" $ do
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                respond $ responseLBS status200 [] teamJson

            resTeam <- getResponseBody <$> getTeamDetail mockBaseRequest dummyAuth (TeamId "testTeamId")
            resTeam `shouldBe` team

            receivedReq <- takeMVar receivedReqMVar
            requestMethod receivedReq `shouldBe` "GET"
            rawPathInfo receivedReq `shouldBe` "/v1/teams/testTeamId"
            (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
            (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"

            stopMockServer svr

        it "getTeamDetailEither returns a (Right Team)" $ do
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                respond $ responseLBS status200 [] teamJson
            (Right resTeam) <- getResponseBody <$> getTeamDetailEither mockBaseRequest dummyAuth (TeamId "testTeamId")
            resTeam `shouldBe` team

            stopMockServer svr

    describe "TeamMemberShip" $ do
        let teamMembershipJson = "{\
                                 \  \"id\" : \"Y2lzY29zcGFyazovL3VzL1RFQU1fTUVNQkVSU0hJUC8wZmNmYTJiOC1hZGNjLTQ1ZWEtYTc4Mi1lNDYwNTkyZjgxZWY6MTNlMThmNDAtNDJmYy0xMWU2LWE5ZDgtMjExYTBkYzc5NzY5\",\
                                 \  \"teamId\" : \"Y2lzY29zcGFyazovL3VzL1RFQU0vMTNlMThmNDAtNDJmYy0xMWU2LWE5ZDgtMjExYTBkYzc5NzY5\",\
                                 \  \"personId\" : \"Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mNWIzNjE4Ny1jOGRkLTQ3MjctOGIyZi1mOWM0NDdmMjkwNDY\",\
                                 \  \"personEmail\" : \"john.andersen@example.com\",\
                                 \  \"personDisplayName\" : \"John Andersen\",\
                                 \  \"personOrgId\" : \"Y2lzY29zcGFyazovL3VzL09SR0FOSVpBVElPTi85NmFiYzJhYS0zZGNjLTExZTUtYTE1Mi1mZTM0ODE5Y2RjOWE\",\
                                 \  \"isModerator\" : true,\
                                 \  \"created\" : \"2015-10-18T14:26:16.057Z\"\
                                 \}"
            teamMembership = TeamMembership { teamMembershipId                  = TeamMembershipId "Y2lzY29zcGFyazovL3VzL1RFQU1fTUVNQkVSU0hJUC8wZmNmYTJiOC1hZGNjLTQ1ZWEtYTc4Mi1lNDYwNTkyZjgxZWY6MTNlMThmNDAtNDJmYy0xMWU2LWE5ZDgtMjExYTBkYzc5NzY5"
                                            , teamMembershipTeamId              = TeamId "Y2lzY29zcGFyazovL3VzL1RFQU0vMTNlMThmNDAtNDJmYy0xMWU2LWE5ZDgtMjExYTBkYzc5NzY5"
                                            , teamMembershipPersonId            = PersonId "Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mNWIzNjE4Ny1jOGRkLTQ3MjctOGIyZi1mOWM0NDdmMjkwNDY"
                                            , teamMembershipPersonEmail         = Email "john.andersen@example.com"
                                            , teamMembershipPersonDisplayName   = DisplayName "John Andersen"
                                            , teamMembershipPersonOrgId         = OrganizationId "Y2lzY29zcGFyazovL3VzL09SR0FOSVpBVElPTi85NmFiYzJhYS0zZGNjLTExZTUtYTE1Mi1mZTM0ODE5Y2RjOWE"
                                            , teamMembershipIsModerator         = True
                                            , teamMembershipCreated             = Timestamp "2015-10-18T14:26:16.057Z"
                                            }
            teamMembershipGen i = TeamMembership { teamMembershipId                  = TeamMembershipId . pack $ "teamMembershipId" <> i
                                                 , teamMembershipTeamId              = TeamId . pack $ "teamId" <> i
                                                 , teamMembershipPersonId            = PersonId . pack $ "personId" <> i
                                                 , teamMembershipPersonEmail         = Email . pack $ "email" <> i <> "@example.com"
                                                 , teamMembershipPersonDisplayName   = DisplayName . pack $ "DisplayName" <> i
                                                 , teamMembershipPersonOrgId         = OrganizationId . pack $ "OrganizationId" <> i
                                                 , teamMembershipIsModerator         = True
                                                 , teamMembershipCreated             = Timestamp . pack $ "Timestamp" <> i
                                                 }
            teamMembershipList j = [ teamMembershipGen $ j <> show i | i <- [1..3] ]
            teamMembershipListList = [ teamMembershipList [c] | c <- ['a'..'d'] ]

        it "streamTeamMembershipList streams TeamMembership" $ do
            let testData = teamMembershipList ['Z']
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp (encode (TeamMembershipList testData)) req respond

            res <- runConduit $ streamTeamMembershipList dummyAuth mockBaseRequest def .| sinkList
            res `shouldBe` testData

            receivedReq <- takeMVar receivedReqMVar
            rawPathInfo receivedReq `shouldBe` "/v1/team/memberships"
            queryString receivedReq `shouldBe` []

            stopMockServer svr

        it "streamMembershipList passes query strings build from TeamMembershipQuery to server" $ do
            let testData = teamMembershipList ['Z']
                teamMembershipQuery = TeamMembershipQuery . Just $ TeamId "DummyTeamId"

            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp (encode (TeamMembershipList testData)) req respond

            res <- runConduit $ streamTeamMembershipList dummyAuth mockBaseRequest teamMembershipQuery .| sinkList
            res `shouldBe` testData

            receivedReq <- takeMVar receivedReqMVar
            rawPathInfo receivedReq `shouldBe` "/v1/team/memberships"
            queryString receivedReq `shouldBe` [ ("teamId", Just "DummyTeamId") ]

            stopMockServer svr

        it "streamTeamMembershipList streams TeamMembership with automatic pagination" $ do
            svr <- startMockServer . paginationApp $ encode . TeamMembershipList <$> teamMembershipListList

            res <- runConduit $ streamTeamMembershipList dummyAuth mockBaseRequest def .| sinkList
            res `shouldBe` concat teamMembershipListList

            stopMockServer svr

        it "getTeamMembershipDetail returns a TeamMembership" $ do
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                respond $ responseLBS status200 [] teamMembershipJson

            resTeamMembership <- getResponseBody <$> getTeamMembershipDetail mockBaseRequest dummyAuth (TeamMembershipId "testTeamMembershipId")
            resTeamMembership `shouldBe` teamMembership

            receivedReq <- takeMVar receivedReqMVar
            requestMethod receivedReq `shouldBe` "GET"
            rawPathInfo receivedReq `shouldBe` "/v1/team/memberships/testTeamMembershipId"
            (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
            (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"

            stopMockServer svr

        it "getTeamMembershipDetailEither returns a (Right TeamMembership)" $ do
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                respond $ responseLBS status200 [] teamMembershipJson
            (Right resTeamMembership) <- getResponseBody <$> getTeamMembershipDetailEither mockBaseRequest dummyAuth (TeamMembershipId "testTeamMembershipId")
            resTeamMembership `shouldBe` teamMembership

            stopMockServer svr

    describe "Room" $ do
        let roomJson = "{\
                       \  \"id\" : \"Y2lzY29zcGFyazovL3VzL1JPT00vYmJjZWIxYWQtNDNmMS0zYjU4LTkxNDctZjE0YmIwYzRkMTU0\",\
                       \  \"title\" : \"Project Unicorn - Sprint 0\",\
                       \  \"type\" : \"group\",\
                       \  \"isLocked\" : true,\
                       \  \"sipAddress\" : \"01234567890@meet.ciscospark.com\",\
                       \  \"teamId\" : \"Y2lzY29zcGFyazovL3VzL1JPT00vNjRlNDVhZTAtYzQ2Yi0xMWU1LTlkZjktMGQ0MWUzNDIxOTcz\",\
                       \  \"lastActivity\" : \"2016-04-21T19:12:48.920Z\",\
                       \  \"creatorId\": \"Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mNWIzNjE4Ny1jOGRkLTQ3MjctOGIyZi1mOWM0NDdmMjkwNDY\",\
                       \  \"created\" : \"2016-04-21T19:01:55.966Z\"\
                       \}"
            room = Room { roomId            = RoomId  "Y2lzY29zcGFyazovL3VzL1JPT00vYmJjZWIxYWQtNDNmMS0zYjU4LTkxNDctZjE0YmIwYzRkMTU0"
                        , roomTitle         = RoomTitle "Project Unicorn - Sprint 0"
                        , roomType          = RoomTypeGroup
                        , roomIsLocked      = True
                        , roomSipAddress    = Just $ SipAddr "01234567890@meet.ciscospark.com"
                        , roomLastActivity  = Timestamp "2016-04-21T19:12:48.920Z"
                        , roomTeamId        = Just $ TeamId "Y2lzY29zcGFyazovL3VzL1JPT00vNjRlNDVhZTAtYzQ2Yi0xMWU1LTlkZjktMGQ0MWUzNDIxOTcz"
                        , roomCreatorId     = PersonId "Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mNWIzNjE4Ny1jOGRkLTQ3MjctOGIyZi1mOWM0NDdmMjkwNDY"
                        , roomCreated       = Timestamp "2016-04-21T19:01:55.966Z"
                        }
            roomGen i = Room { roomId           = RoomId . pack $ "roomId" <> i
                             , roomTitle        = RoomTitle . pack $ "roomTitle" <> i
                             , roomType         = RoomTypeGroup
                             , roomIsLocked     = True
                             , roomSipAddress   = Just $ SipAddr . pack $ "rooomSipAddress" <> i <> "@meet.ciscospark.com"
                             , roomLastActivity = Timestamp . pack $ "roomLastActivity" <> i
                             , roomTeamId       = Just $ TeamId . pack $ "roomTeamId" <> i
                             , roomCreatorId    = PersonId . pack $ "personId" <> i
                             , roomCreated      = Timestamp .pack $ "roomCreated" <> i
                             }
            roomList j = [ roomGen $ j <> show i | i <- [1..3] ]
            roomListList = [ roomList [c] | c <- ['a'..'d'] ]

        it "streamRoomList streams Room" $ do
            let testData = roomList ['Z']
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp (encode (RoomList testData)) req respond

            res <- runConduit $ streamRoomList dummyAuth mockBaseRequest def .| sinkList
            res `shouldBe` testData

            receivedReq <- takeMVar receivedReqMVar
            rawPathInfo receivedReq `shouldBe` "/v1/rooms"
            queryString receivedReq `shouldBe` []

            stopMockServer svr

        it "streamRoomList passes query strings build from RoomQuery to server" $ do
            let testData = roomList ['Z']
                roomQuery = RoomQuery (Just $ TeamId "dummyTeamId")
                                      (Just RoomTypeGroup)
                                      (Just RoomQuerySortByLastActivity)

            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp (encode (RoomList testData)) req respond

            res <- runConduit $ streamRoomList dummyAuth mockBaseRequest roomQuery .| sinkList
            res `shouldBe` testData

            receivedReq <- takeMVar receivedReqMVar
            rawPathInfo receivedReq `shouldBe` "/v1/rooms"
            (sort . queryString) receivedReq `shouldBe` sort [ ("type", Just "group")
                                                             , ("sortBy", Just "lastactivity")
                                                             , ("teamId", Just "dummyTeamId") ]

            stopMockServer svr

        it "streamRoomList streams Room with automatic pagination" $ do
            svr <- startMockServer . paginationApp $ encode . RoomList <$> roomListList

            res <- runConduit $ streamRoomList dummyAuth mockBaseRequest def .| sinkList
            res `shouldBe` concat roomListList

            stopMockServer svr

        it "getRoomDetail returns a Room" $ do
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                respond $ responseLBS status200 [] roomJson

            resRoom <- getResponseBody <$> getRoomDetail mockBaseRequest dummyAuth (RoomId "testRoomId")
            resRoom `shouldBe` room

            receivedReq <- takeMVar receivedReqMVar
            requestMethod receivedReq `shouldBe` "GET"
            rawPathInfo receivedReq `shouldBe` "/v1/rooms/testRoomId"
            (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
            (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"

            stopMockServer svr

        it "getRoomDetailEither returns a (Right Room)" $ do
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                respond $ responseLBS status200 [] roomJson
            (Right resRoom) <- getResponseBody <$> getRoomDetailEither mockBaseRequest dummyAuth (RoomId "testRoomId")
            resRoom `shouldBe` room

            stopMockServer svr

    describe "Membership" $ do
        let membershipJson = "{\
                             \  \"id\" : \"Y2lzY29zcGFyazovL3VzL01FTUJFUlNISVAvMGQwYzkxYjYtY2U2MC00NzI1LWI2ZDAtMzQ1NWQ1ZDExZWYzOmNkZTFkZDQwLTJmMGQtMTFlNS1iYTljLTdiNjU1NmQyMjA3Yg\",\
                             \  \"roomId\" : \"Y2lzY29zcGFyazovL3VzL1JPT00vYmJjZWIxYWQtNDNmMS0zYjU4LTkxNDctZjE0YmIwYzRkMTU0\",\
                             \  \"personId\" : \"Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mNWIzNjE4Ny1jOGRkLTQ3MjctOGIyZi1mOWM0NDdmMjkwNDY\",\
                             \  \"personEmail\" : \"john.andersen@example.com\",\
                             \  \"personDisplayName\" : \"John Andersen\",\
                             \  \"personOrgId\" : \"Y2lzY29zcGFyazovL3VzL09SR0FOSVpBVElPTi85NmFiYzJhYS0zZGNjLTExZTUtYTE1Mi1mZTM0ODE5Y2RjOWE\",\
                             \  \"isModerator\" : true,\
                             \  \"isMonitor\" : true,\
                             \  \"created\" : \"2015-10-18T14:26:16.203Z\"\
                             \}"
            membership = Membership { membershipId                  = MembershipId "Y2lzY29zcGFyazovL3VzL01FTUJFUlNISVAvMGQwYzkxYjYtY2U2MC00NzI1LWI2ZDAtMzQ1NWQ1ZDExZWYzOmNkZTFkZDQwLTJmMGQtMTFlNS1iYTljLTdiNjU1NmQyMjA3Yg"
                                    , membershipRoomId              = RoomId "Y2lzY29zcGFyazovL3VzL1JPT00vYmJjZWIxYWQtNDNmMS0zYjU4LTkxNDctZjE0YmIwYzRkMTU0"
                                    , membershipPersonId            = PersonId "Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mNWIzNjE4Ny1jOGRkLTQ3MjctOGIyZi1mOWM0NDdmMjkwNDY"
                                    , membershipPersonEmail         = Email "john.andersen@example.com"
                                    , membershipPersonDisplayName   = DisplayName "John Andersen"
                                    , membershipPersonOrgId         = OrganizationId "Y2lzY29zcGFyazovL3VzL09SR0FOSVpBVElPTi85NmFiYzJhYS0zZGNjLTExZTUtYTE1Mi1mZTM0ODE5Y2RjOWE"
                                    , membershipIsModerator         = True
                                    , membershipIsMonitor           = True
                                    , membershipCreated             = Timestamp "2015-10-18T14:26:16.203Z"
                                    }
            membershipGen i = Membership { membershipId                 = MembershipId . pack $ "membershipId" <> i
                                         , membershipRoomId             = RoomId . pack $ "roomId" <> i
                                         , membershipPersonId           = PersonId . pack $ "personId" <> i
                                         , membershipPersonEmail        = Email . pack $ "email" <> i <> "@example.com"
                                         , membershipPersonDisplayName  = DisplayName . pack $ "displayName" <> i
                                         , membershipPersonOrgId        = OrganizationId . pack $ "orgId" <> i
                                         , membershipIsModerator        = True
                                         , membershipIsMonitor          = True
                                         , membershipCreated            = Timestamp . pack $ "timestamp" <> i
                                         }
            membershipList j = [ membershipGen $ j <> show i | i <- [1..3] ]
            membershipListList = [ membershipList [c] | c <- ['a'..'d'] ]

        it "streamMembershipList streams Membership" $ do
            let testData = membershipList ['Z']
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp (encode (MembershipList testData)) req respond

            res <- runConduit $ streamMembershipList dummyAuth mockBaseRequest def .| sinkList
            res `shouldBe` testData

            receivedReq <- takeMVar receivedReqMVar
            rawPathInfo receivedReq `shouldBe` "/v1/memberships"
            queryString receivedReq `shouldBe` []

            stopMockServer svr

        it "streamMembershipList passes query strings build from MembershipQuery to server" $ do
            let testData = membershipList ['Z']
                membershipQuery = MembershipQuery (Just $ RoomId "dummyRoomId")
                                                  (Just $ PersonId "personIdQuery")
                                                  (Just $ Email "personEmailQuery")

            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp (encode (MembershipList testData)) req respond

            res <- runConduit $ streamMembershipList dummyAuth mockBaseRequest membershipQuery .| sinkList
            res `shouldBe` testData

            receivedReq <- takeMVar receivedReqMVar
            rawPathInfo receivedReq `shouldBe` "/v1/memberships"
            (sort . queryString) receivedReq `shouldBe` sort [ ("personId", Just "personIdQuery")
                                                             , ("personEmail", Just "personEmailQuery")
                                                             , ("roomId", Just "dummyRoomId") ]

            stopMockServer svr

        it "streamMembershipList streams Membership with automatic pagination" $ do
            svr <- startMockServer . paginationApp $ encode . MembershipList <$> membershipListList

            res <- runConduit $ streamMembershipList dummyAuth mockBaseRequest def .| sinkList
            res `shouldBe` concat membershipListList

            stopMockServer svr

        it "getMembershipDetail returns a Membership" $ do
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                respond $ responseLBS status200 [] membershipJson

            resMembership <- getResponseBody <$> getMembershipDetail mockBaseRequest dummyAuth (MembershipId "testMembershipId")
            resMembership `shouldBe` membership

            receivedReq <- takeMVar receivedReqMVar
            requestMethod receivedReq `shouldBe` "GET"
            rawPathInfo receivedReq `shouldBe` "/v1/memberships/testMembershipId"
            (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
            (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"

            stopMockServer svr

        it "getMembershipDetailEither returns a (Right Membership)" $ do
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                respond $ responseLBS status200 [] membershipJson
            (Right resMembership) <- getResponseBody <$> getMembershipDetailEither mockBaseRequest dummyAuth (MembershipId "testMembershipId")
            resMembership `shouldBe` membership

            stopMockServer svr

    describe "Message" $ do
        let messageJson = "{\
                          \  \"id\" : \"Y2lzY29zcGFyazovL3VzL01FU1NBR0UvOTJkYjNiZTAtNDNiZC0xMWU2LThhZTktZGQ1YjNkZmM1NjVk\",\
                          \  \"roomId\" : \"Y2lzY29zcGFyazovL3VzL1JPT00vYmJjZWIxYWQtNDNmMS0zYjU4LTkxNDctZjE0YmIwYzRkMTU0\",\
                          \  \"roomType\" : \"group\",\
                          \  \"toPersonId\" : \"Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mMDZkNzFhNS0wODMzLTRmYTUtYTcyYS1jYzg5YjI1ZWVlMmX\",\
                          \  \"toPersonEmail\" : \"julie@example.com\",\
                          \  \"text\" : \"PROJECT UPDATE - A new project plan has been published on Box: http://box.com/s/lf5vj. The PM for this project is Mike C. and the Engineering Manager is Jane W.\",\
                          \  \"html\" : \"<h1>HTML formatted message goes here</h1><p>when the message was posted in markdown format.</p>\",\
                          \  \"files\" : [ \"http://www.example.com/images/media.png\" ],\
                          \  \"personId\" : \"Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mNWIzNjE4Ny1jOGRkLTQ3MjctOGIyZi1mOWM0NDdmMjkwNDY\",\
                          \  \"personEmail\" : \"matt@example.com\",\
                          \  \"created\" : \"2015-10-18T14:26:16+00:00\",\
                          \  \"mentionedPeople\" : [ \"Y2lzY29zcGFyazovL3VzL1BFT1BMRS8yNDlmNzRkOS1kYjhhLTQzY2EtODk2Yi04NzllZDI0MGFjNTM\", \"Y2lzY29zcGFyazovL3VzL1BFT1BMRS83YWYyZjcyYy0xZDk1LTQxZjAtYTcxNi00MjlmZmNmYmM0ZDg\" ]\
                          \}"
            message = Message { messageId               = MessageId "Y2lzY29zcGFyazovL3VzL01FU1NBR0UvOTJkYjNiZTAtNDNiZC0xMWU2LThhZTktZGQ1YjNkZmM1NjVk"
                              , messageRoomId           = RoomId "Y2lzY29zcGFyazovL3VzL1JPT00vYmJjZWIxYWQtNDNmMS0zYjU4LTkxNDctZjE0YmIwYzRkMTU0"
                              , messageRoomType         = RoomTypeGroup
                              , messageToPersonId       = Just $ PersonId "Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mMDZkNzFhNS0wODMzLTRmYTUtYTcyYS1jYzg5YjI1ZWVlMmX"
                              , messageToPersonEmail    = Just $ Email "julie@example.com"
                              , messageText             = MessageText "PROJECT UPDATE - A new project plan has been published on Box: http://box.com/s/lf5vj. The PM for this project is Mike C. and the Engineering Manager is Jane W."
                              , messageHtml             = Just $ MessageHtml "<h1>HTML formatted message goes here</h1><p>when the message was posted in markdown format.</p>"
                              , messageFiles            = Just $ [ FileUrl "http://www.example.com/images/media.png" ]
                              , messagePersonId         = PersonId "Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mNWIzNjE4Ny1jOGRkLTQ3MjctOGIyZi1mOWM0NDdmMjkwNDY"
                              , messagePersonEmail      = Email "matt@example.com"
                              , messageCreated          = Timestamp "2015-10-18T14:26:16+00:00"
                              , messageMentionedPeople  = Just $ [ PersonId "Y2lzY29zcGFyazovL3VzL1BFT1BMRS8yNDlmNzRkOS1kYjhhLTQzY2EtODk2Yi04NzllZDI0MGFjNTM"
                                                                 , PersonId "Y2lzY29zcGFyazovL3VzL1BFT1BMRS83YWYyZjcyYy0xZDk1LTQxZjAtYTcxNi00MjlmZmNmYmM0ZDg" ]
                              }
            messageGen i = Message { messageId               = MessageId . pack $ "messageId" <> i
                                   , messageRoomId           = RoomId . pack $ "roomId" <> i
                                   , messageRoomType         = RoomTypeGroup
                                   , messageToPersonId       = Just . PersonId . pack $ "toPersonId" <> i
                                   , messageToPersonEmail    = Just . Email . pack $ "julie" <> i <> "@example.com"
                                   , messageText             = MessageText . pack $ "messageText" <> i
                                   , messageHtml             = Just . MessageHtml . pack $ "messageHtml" <> i
                                   , messageFiles            = Just $ [ FileUrl . pack $ "http://www.example.com/images/media" <> i <> ".png" ]
                                   , messagePersonId         = PersonId . pack $ "personId" <> i
                                   , messagePersonEmail      = Email . pack $ "matt" <> i <> "@example.com"
                                   , messageCreated          = Timestamp . pack $ "created" <> i
                                   , messageMentionedPeople  = Just $ [ PersonId . pack $ "memtionedPeople1-" <> i
                                                                      , PersonId . pack $ "memtionedPeople2-" <> i ]
                                   }
            messageList j = [ messageGen $ j <> show i | i <- [1..3] ]
            messageListList = [ messageList [c] | c <- ['a'..'d'] ]
            defQuery = defaultMessageQuery $ RoomId "dummyRoomId"

        it "streamMessageList streams Message" $ do
            let testData = messageList ['Z']
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp (encode (MessageList testData)) req respond

            res <- runConduit $ streamMessageList dummyAuth mockBaseRequest defQuery .| sinkList
            res `shouldBe` testData

            receivedReq <- takeMVar receivedReqMVar
            rawPathInfo receivedReq `shouldBe` "/v1/messages"
            queryString receivedReq `shouldBe` [ ("roomId", Just "dummyRoomId") ]

            stopMockServer svr

        it "streamMessageList passes query strings build from MessageQuery to server" $ do
            let testData = messageList ['Z']
                messageQuery = MessageQuery (RoomId "dummyRoomId")
                                            (Just $ MentionedPeopleMe)
                                            (Just $ Timestamp "beforeQuery")
                                            (Just $ MessageId "beforeMessageQuery")

            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp (encode (MessageList testData)) req respond

            res <- runConduit $ streamMessageList dummyAuth mockBaseRequest messageQuery .| sinkList
            res `shouldBe` testData

            receivedReq <- takeMVar receivedReqMVar
            rawPathInfo receivedReq `shouldBe` "/v1/messages"
            (sort . queryString) receivedReq `shouldBe` sort [ ("roomId", Just "dummyRoomId")
                                                             , ("mentionedPeople", Just "me")
                                                             , ("beforeMessage", Just "beforeMessageQuery")
                                                             , ("before", Just "beforeQuery") ]

            stopMockServer svr

        it "streamMessageList streams Message with automatic pagination" $ do
            svr <- startMockServer . paginationApp $ encode . MessageList <$> messageListList

            res <- runConduit $ streamMessageList dummyAuth mockBaseRequest defQuery .| sinkList
            res `shouldBe` concat messageListList

            stopMockServer svr

        it "getMessageDetail returns a Message" $ do
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                respond $ responseLBS status200 [] messageJson

            resMessage <- getResponseBody <$> getMessageDetail mockBaseRequest dummyAuth (MessageId "testMessageId")
            resMessage `shouldBe` message

            receivedReq <- takeMVar receivedReqMVar
            requestMethod receivedReq `shouldBe` "GET"
            rawPathInfo receivedReq `shouldBe` "/v1/messages/testMessageId"
            (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
            (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"

            stopMockServer svr

        it "getMessageDetailEither returns a (Right Message)" $ do
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                respond $ responseLBS status200 [] messageJson
            (Right resMessage) <- getResponseBody <$> getMessageDetailEither mockBaseRequest dummyAuth (MessageId "testMessageId")
            resMessage `shouldBe` message

            stopMockServer svr



        it "Message tests" $ do
            pending

    describe "Organization" $ do
        let organizationJson = "{\
                               \  \"id\" : \"OTZhYmMyYWEtM2RjYy0xMWU1LWExNTItZmUzNDgxOWNkYzlh\",\
                               \  \"displayName\" : \"Cisco, Inc.\",\
                               \  \"created\" : \"2015-10-18T14:26:16+00:00\"\
                               \}"
            organization = Organization { organizationId            = OrganizationId "OTZhYmMyYWEtM2RjYy0xMWU1LWExNTItZmUzNDgxOWNkYzlh"
                                        , organizationDisplayName   = OrganizationDisplayName "Cisco, Inc."
                                        , organizationCreated       = Timestamp "2015-10-18T14:26:16+00:00"
                                        }
            organizationGen i = Organization { organizationId            = OrganizationId . pack $ "organizationId" <> i
                                             , organizationDisplayName   = OrganizationDisplayName . pack $ "displayName" <> i
                                             , organizationCreated       = Timestamp . pack $ "timestamp" <> i
                                             }
            organizationList j = [ organizationGen $ j <> show i | i <- [1..3] ]
            organizationListList = [ organizationList [c] | c <- ['a'..'d'] ]

        it "streamOrganizationList streams Organization" $ do
            let testData = organizationList ['Z']
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp (encode (OrganizationList testData)) req respond

            res <- runConduit $ streamOrganizationList dummyAuth mockBaseRequest .| sinkList
            res `shouldBe` testData

            receivedReq <- takeMVar receivedReqMVar
            rawPathInfo receivedReq `shouldBe` "/v1/organizations"
            queryString receivedReq `shouldBe` []

            stopMockServer svr

        it "streamOrganizationList streams Organization with automatic pagination" $ do
            svr <- startMockServer . paginationApp $ encode . OrganizationList <$> organizationListList

            res <- runConduit $ streamOrganizationList dummyAuth mockBaseRequest .| sinkList
            res `shouldBe` concat organizationListList

            stopMockServer svr

        it "getOrganizationDetail returns a Organization" $ do
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                respond $ responseLBS status200 [] organizationJson

            resOrganization <- getResponseBody <$> getOrganizationDetail mockBaseRequest dummyAuth (OrganizationId "testOrganizationId")
            resOrganization `shouldBe` organization

            receivedReq <- takeMVar receivedReqMVar
            requestMethod receivedReq `shouldBe` "GET"
            rawPathInfo receivedReq `shouldBe` "/v1/organizations/testOrganizationId"
            (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
            (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"

            stopMockServer svr

        it "getOrganizationDetailEither returns a (Right Organization)" $ do
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                respond $ responseLBS status200 [] organizationJson
            (Right resOrganization) <- getResponseBody <$> getOrganizationDetailEither mockBaseRequest dummyAuth (OrganizationId "testOrganizationId")
            resOrganization `shouldBe` organization

            stopMockServer svr

    describe "License" $ do
        let licenseJson = "{\
                          \  \"id\" : \"OTZhYmMyYWEtM2RjYy0xMWU1LWExNTItZmUzNDgxOWNkYzlh\",\
                          \  \"name\" : \"Spark Calling\",\
                          \  \"totalUnits\" : 42,\
                          \  \"consumedUnits\" : 8\
                          \}"
            license = License { licenseId               = LicenseId "OTZhYmMyYWEtM2RjYy0xMWU1LWExNTItZmUzNDgxOWNkYzlh"
                              , licenseName             = LicenseName "Spark Calling"
                              , licenseTotalUnits       = LicenseUnit 42
                              , licenseConsumedUnits    = LicenseUnit 8
                              }
            licenseGen i = License { licenseId               = LicenseId . pack $ "licenseId" <> i
                                   , licenseName             = LicenseName . pack $ "licenseName" <> i
                                   , licenseTotalUnits       = LicenseUnit 42
                                   , licenseConsumedUnits    = LicenseUnit 8
                                   }
            licenseList j = [ licenseGen $ j <> show i | i <- [1..3] ]
            licenseListList = [ licenseList [c] | c <- ['a'..'d'] ]

        it "streamLicenseList streams License" $ do
            let testData = licenseList ['Z']
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp (encode (LicenseList testData)) req respond

            res <- runConduit $ streamLicenseList dummyAuth mockBaseRequest def .| sinkList
            res `shouldBe` testData

            receivedReq <- takeMVar receivedReqMVar
            rawPathInfo receivedReq `shouldBe` "/v1/licenses"
            queryString receivedReq `shouldBe` []

            stopMockServer svr

        it "streamLicenseList passes query strings build from LicenseQuery to server" $ do
            let testData = licenseList ['Z']
                licenseQuery = LicenseQuery (Just $ OrganizationId "orgIdQuery")

            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp (encode (LicenseList testData)) req respond

            res <- runConduit $ streamLicenseList dummyAuth mockBaseRequest licenseQuery .| sinkList
            res `shouldBe` testData

            receivedReq <- takeMVar receivedReqMVar
            rawPathInfo receivedReq `shouldBe` "/v1/licenses"
            queryString receivedReq `shouldBe` [ ("orgId", Just "orgIdQuery") ]

            stopMockServer svr

        it "streamLicenseList streams License with automatic pagination" $ do
            svr <- startMockServer . paginationApp $ encode . LicenseList <$> licenseListList

            res <- runConduit $ streamLicenseList dummyAuth mockBaseRequest def .| sinkList
            res `shouldBe` concat licenseListList

            stopMockServer svr

        it "getLicenseDetail returns a License" $ do
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                respond $ responseLBS status200 [] licenseJson

            resLicense <- getResponseBody <$> getLicenseDetail mockBaseRequest dummyAuth (LicenseId "testLicenseId")
            resLicense `shouldBe` license

            receivedReq <- takeMVar receivedReqMVar
            requestMethod receivedReq `shouldBe` "GET"
            rawPathInfo receivedReq `shouldBe` "/v1/licenses/testLicenseId"
            (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
            (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"

            stopMockServer svr

        it "getLicenseDetailEither returns a (Right License)" $ do
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                respond $ responseLBS status200 [] licenseJson
            (Right resLicense) <- getResponseBody <$> getLicenseDetailEither mockBaseRequest dummyAuth (LicenseId "testLicenseId")
            resLicense `shouldBe` license

            stopMockServer svr

    describe "Role" $ do
        let roleJson = "{\
                       \  \"id\" : \"OTZhYmMyYWEtM2RjYy0xMWU1LWExNTItZmUzNDgxOWNkYzlh\",\
                       \  \"name\" : \"Full Administrator\"\
                       \}"
            role = Role { roleId    = RoleId "OTZhYmMyYWEtM2RjYy0xMWU1LWExNTItZmUzNDgxOWNkYzlh"
                        , roleName  = RoleName "Full Administrator"
                        }
            roleGen i = Role { roleId   = RoleId . pack $ "roleId" <> i
                             , roleName = RoleName . pack $ "roleName" <> i
                             }
            roleList j = [ roleGen $ j <> show i | i <- [1..3] ]
            roleListList = [ roleList [c] | c <- ['a'..'d'] ]

        it "streamRoleList streams Role" $ do
            let testData = roleList ['Z']
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp (encode (RoleList testData)) req respond

            res <- runConduit $ streamRoleList dummyAuth mockBaseRequest .| sinkList
            res `shouldBe` testData

            receivedReq <- takeMVar receivedReqMVar
            rawPathInfo receivedReq `shouldBe` "/v1/roles"
            queryString receivedReq `shouldBe` []

            stopMockServer svr

        it "streamRoleList streams Role with automatic pagination" $ do
            svr <- startMockServer . paginationApp $ encode . RoleList <$> roleListList

            res <- runConduit $ streamRoleList dummyAuth mockBaseRequest .| sinkList
            res `shouldBe` concat roleListList

            stopMockServer svr

        it "getRoleDetail returns a Role" $ do
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                respond $ responseLBS status200 [] roleJson

            resRole <- getResponseBody <$> getRoleDetail mockBaseRequest dummyAuth (RoleId "testRoleId")
            resRole `shouldBe` role

            receivedReq <- takeMVar receivedReqMVar
            requestMethod receivedReq `shouldBe` "GET"
            rawPathInfo receivedReq `shouldBe` "/v1/roles/testRoleId"
            (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
            (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"

            stopMockServer svr

        it "getRoleDetailEither returns a (Right Role)" $ do
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                respond $ responseLBS status200 [] roleJson
            (Right resRole) <- getResponseBody <$> getRoleDetailEither mockBaseRequest dummyAuth (RoleId "testRoleId")
            resRole `shouldBe` role

            stopMockServer svr
