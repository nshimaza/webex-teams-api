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

mockBaseRequest :: C.Request
mockBaseRequest
    = C.addRequestHeader "Content-Type" "application/json; charset=utf-8"
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
teamList j = [ teamGen $ j ++ show i | i <- [1..3] ]

teamListList :: [[Team]]
teamListList = [ teamList [c] | c <- ['a'..'d'] ]

spec :: Spec
spec = do
    describe "Mock Applications" $ do
        it "simple mock app returns list of team" $ do
            receivedReqMVar <- newEmptyMVar
            let req = setRequestPath ("/v1/teams")
                    $ setRequestMethod "GET"
                    $ mockBaseRequest
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
            let req = setRequestPath ("/v1/teams")
                    $ setRequestMethod "GET"
                    $ mockBaseRequest
                testData = map (\tl -> encode $ TeamList tl) teamListList

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                paginationApp testData req respond

            res1 <- httpJSON req
            getResponseBody res1 `shouldBe` TeamList (teamListList !! 0)

            receivedReq <- takeMVar receivedReqMVar
            rawPathInfo receivedReq `shouldBe` "/v1/teams"

            let path = getNextUrl res1
            path `shouldBe` Just "http://localhost:3000/1"

            req2 <- parseRequest $ "GET " <> (C8.unpack $ fromJust path)
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
            personList j = [ personGen $ j ++ show i | i <- [1..3] ]
            personListList = [ personList [c] | c <- ['a'..'d'] ]

        it "streamPersonList streams Team" $ do
            let testData = personList $ ['Z']
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp (encode (PersonList testData)) req respond

            res <- runConduit $ streamPersonList dummyAuth mockBaseRequest defaultPersonQuery .| sinkList
            res `shouldBe` testData

            receivedReq <- takeMVar receivedReqMVar
            rawPathInfo receivedReq `shouldBe` "/v1/people"
            queryString receivedReq `shouldBe` []

            stopMockServer svr

        it "streamPersonList passes query strings build from PersonQuery to server" $ do
            let testData = personList $ ['Z']
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
            svr <- startMockServer $ paginationApp $ map (\pl -> encode $ PersonList pl) personListList

            res <- runConduit $ streamPersonList dummyAuth mockBaseRequest defaultPersonQuery .| sinkList
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
            let testData = teamList $ ['Z']
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
            svr <- startMockServer $ paginationApp $ map (\tl -> encode $ TeamList tl) teamListList

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
            teamMembershipList j = [ teamMembershipGen $ j ++ show i | i <- [1..3] ]
            teamMembershipListList = [ teamMembershipList [c] | c <- ['a'..'d'] ]

        it "streamTeamMembershipList streams TeamMembership" $ do
            let testData = teamMembershipList $ ['Z']
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp (encode (TeamMembershipList testData)) req respond

            res <- runConduit $ streamTeamMembershipList dummyAuth mockBaseRequest defaultTeamMembershipQuery .| sinkList
            res `shouldBe` testData

            receivedReq <- takeMVar receivedReqMVar
            rawPathInfo receivedReq `shouldBe` "/v1/team/memberships"
            queryString receivedReq `shouldBe` []

            stopMockServer svr


        it "streamMembershipList passes query strings build from TeamMembershipQuery to server" $ do
            let testData = teamMembershipList $ ['Z']
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
            svr <- startMockServer $ paginationApp $ map (\pl -> encode $ TeamMembershipList pl) teamMembershipListList

            res <- runConduit $ streamTeamMembershipList dummyAuth mockBaseRequest defaultTeamMembershipQuery .| sinkList
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

    describe "WaiTest" $ do
        it "start and stop server" $ do
--             svr <- startMockServer helloApp
--             threadDelay (30 * 1000000)
--             stopMockServer svr
--             svr <- startMockServer helloApp
--             stopMockServer svr
            pending


