{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.CiscoSparkSpec where

import           Conduit
import           Control.Concurrent.MVar      (MVar, newEmptyMVar, putMVar,
                                               takeMVar)
import           Control.Monad                (void)
import           Data.Aeson                   (decode, encode)
import           Data.Attoparsec.ByteString   (parseOnly)
import qualified Data.ByteString              as S (ByteString)
import qualified Data.ByteString.Char8        as C8 (unpack)
import qualified Data.ByteString.Lazy         as L (ByteString)
import           Data.Default                 (def)
import           Data.List                    (sort)
import           Data.Maybe                   (fromJust)
import           Data.Monoid                  ((<>))
import           Data.Text                    (pack)

import           Network.HTTP.Simple          as C

import           Network.HTTP.Types           (Header, status200)
import           Network.URI                  (URIAuth (..))
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

mockBaseRequestRequest
    = C.addRequestHeader "Content-Type" "application/json; charset=utf-8"
    $ C.setRequestPort 3000
    $ C.setRequestHost "localhost"
    $ C.setRequestSecure False
    $ C.defaultRequest

mockBaseRequest :: CiscoSparkRequest
mockBaseRequest = CiscoSparkRequest mockBaseRequestRequest "http:" $ URIAuth "" "localhost" ":3000"

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

invalidPaginationApp :: [L.ByteString] -> Application
invalidPaginationApp ress req respond = do
    let (cTypes, body) = dispatch $ rawPathInfo req
    respond $ responseLBS status200 cTypes body
      where
        dispatch "/1"   = ([contentType, ("Link", "<http://localhost:8888/2>; rel=\"next\"")], ress !! 1)
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
            let (CiscoSparkRequest baseReq _ _) = mockBaseRequest
                req = setRequestPath "/v1/teams"
                    $ setRequestMethod "GET"
                    $ baseReq
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
            let (CiscoSparkRequest baseReq _ _) = mockBaseRequest
                req = setRequestPath "/v1/teams"
                    $ setRequestMethod "GET"
                    $ baseReq
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
            newPerson = CreatePerson { createPersonEmails       = Just $ [Email "johnny.chang@foomail.com", Email "jchang@barmail.com"]
                                     , createPersonDisplayName  = Just $ DisplayName "John Andersen"
                                     , createPersonFirstName    = Just (FirstName "John")
                                     , createPersonLastName     = Just (LastName "Andersen")
                                     , createPersonAvatar       = Just (AvatarUrl "https://1efa7a94ed21783e352-c62266528714497a17239ececf39e9e2.ssl.cf1.rackcdn.com/V1~54c844c89e678e5a7b16a306bc2897b9~wx29yGtlTpilEFlYzqPKag==~1600")
                                     , createPersonOrgId        = Just $ OrganizationId "Y2lzY29zcGFyazovL3VzL09SR0FOSVpBVElPTi85NmFiYzJhYS0zZGNjLTExZTUtYTE1Mi1mZTM0ODE5Y2RjOWE"
                                     , createPersonRoles        = Just [ RoleId "Y2lzY29zcGFyazovL3VzL1JPT00vOGNkYzQwYzQtZjA5ZS0zY2JhLThjMjYtZGQwZTcwYWRlY2Iy"
                                                                       , RoleId "Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mMDZkNzFhNS0wODMzLTRmYTUtYTcyYS1jYzg5YjI1ZWVlMmX"]
                                     , createPersonLicenses     = Just [ LicenseId "Y2lzY29zcGFyazovL3VzL1JPT00vOGNkYzQwYzQtZjA5ZS0zY2JhLThjMjYtZGQwZTcwYWRlY2Iy"
                                                                       , LicenseId "Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mMDZkNzFhNS0wODMzLTRmYTUtYTcyYS1jYzg5YjI1ZWVlMmX"]
                                     }
            updatePerson = UpdatePerson { updatePersonDisplayName   = Just $ DisplayName "New John Andersen"
                                        , updatePersonFirstName     = Just $ FirstName "New John"
                                        , updatePersonLastName      = Just $ LastName "New Andersen"
                                        , updatePersonAvatar        = Just $ AvatarUrl "https://example.com/newAvatarUrl"
                                        , updatePersonOrgId         = Just $ OrganizationId "Y2lzY29zcGFyazovL3VzL09SR0FOSVpBVElPTi85NmFiYzJhYS0zZGNjLTExZTUtYTE1Mi1mZTM0ODE5Y2RjOWE"
                                        , updatePersonRoles         = Just $ [ RoleId "newRoleId1", RoleId "newRoleId2" ]
                                        , updatePersonLicenses      = Just $ [ LicenseId "newLicenseId1", LicenseId "newLicenseId2" ]
                                        }

        it "streamPersonList streams Team" $ do
            let testData = personList ['Z']
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp (encode (PersonList testData)) req respond

            res <- runConduit $ streamEntityWithFilter dummyAuth mockBaseRequest (def :: PersonFilter) .| sinkList
            res `shouldBe` testData

            receivedReq <- takeMVar receivedReqMVar
            rawPathInfo receivedReq `shouldBe` "/v1/people"
            queryString receivedReq `shouldBe` []

            stopMockServer svr

        it "streamPersonList passes query strings build from PersonFilter to server" $ do
            let testData = personList ['Z']
                personFilter = PersonFilter (Just $ Email "person@filter.com")
                                            (Just $ DisplayName "DisplayNameFilter")
                                            (Just $ OrganizationId "OrgIdFilter")

            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp (encode (PersonList testData)) req respond

            res <- runConduit $ streamEntityWithFilter dummyAuth mockBaseRequest personFilter .| sinkList
            res `shouldBe` testData

            receivedReq <- takeMVar receivedReqMVar
            rawPathInfo receivedReq `shouldBe` "/v1/people"
            (sort . queryString) receivedReq `shouldBe` sort [ ("orgId", Just "OrgIdFilter")
                                                             , ("displayName", Just "DisplayNameFilter")
                                                             , ("email", Just "person@filter.com") ]

            stopMockServer svr

        it "streamPersonList streams Team with automatic pagination" $ do
            svr <- startMockServer . paginationApp $ encode . PersonList <$> personListList

            res <- runConduit $ streamEntityWithFilter dummyAuth mockBaseRequest (def :: PersonFilter) .| sinkList
            res `shouldBe` concat personListList

            stopMockServer svr

        it "streamPersonList stops pagination at invalid Link Header" $ do
            svr <- startMockServer . invalidPaginationApp $ encode . PersonList <$> personListList

            res <- runConduit $ streamEntityWithFilter dummyAuth mockBaseRequest (def :: PersonFilter) .| sinkList
            res `shouldBe` concat (take 2 personListList)

            stopMockServer svr

        it "getDetail for a person returns a Person" $ do
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp personJson1 req respond

            resPerson <- getResponseBody <$> getDetail dummyAuth mockBaseRequest (PersonId "testPersonId")
            resPerson `shouldBe` person1

            receivedReq <- takeMVar receivedReqMVar
            requestMethod receivedReq `shouldBe` "GET"
            rawPathInfo receivedReq `shouldBe` "/v1/people/testPersonId"
            (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
            (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"

            stopMockServer svr

        it "getDetailEither for a person returns a (Right Person)" $ do
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp personJson1 req respond

            (Right resPerson) <- getResponseBody <$> getDetailEither dummyAuth mockBaseRequest (PersonId "testPersonId")
            resPerson `shouldBe` person1

            stopMockServer svr

        it "createEntity for a person sends JSON encoded CreatePerson as its body of POST request" $ do
            receivedReqMVar <- newEmptyMVar
            receivedBodyMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                strictRequestBody req >>= putMVar receivedBodyMVar
                simpleApp personJson1 req respond

            resPerson <- getResponseBody <$> createEntity dummyAuth mockBaseRequest newPerson
            resPerson `shouldBe` person1

            receivedReq <- takeMVar receivedReqMVar
            receivedBody <- takeMVar receivedBodyMVar
            requestMethod receivedReq `shouldBe` "POST"
            rawPathInfo receivedReq `shouldBe` "/v1/people"
            (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
            (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"
            decode receivedBody `shouldBe` Just newPerson

            stopMockServer svr

        it "createEntityEither for a person sends JSON encoded CreatePerson as its body of POST request" $ do
            receivedReqMVar <- newEmptyMVar
            receivedBodyMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                strictRequestBody req >>= putMVar receivedBodyMVar
                simpleApp personJson1 req respond

            (Right resPerson) <- getResponseBody <$> createEntityEither dummyAuth mockBaseRequest newPerson
            resPerson `shouldBe` person1

            receivedReq <- takeMVar receivedReqMVar
            receivedBody <- takeMVar receivedBodyMVar
            requestMethod receivedReq `shouldBe` "POST"
            rawPathInfo receivedReq `shouldBe` "/v1/people"
            (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
            (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"
            decode receivedBody `shouldBe` Just newPerson

            stopMockServer svr

        it "updateEntity for a person sends JSON encoded UpdatePerson as its body of PUT request" $ do
            receivedReqMVar <- newEmptyMVar
            receivedBodyMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                strictRequestBody req >>= putMVar receivedBodyMVar
                simpleApp personJson1 req respond

            resPerson <- getResponseBody <$> updateEntity dummyAuth mockBaseRequest updatePerson
            resPerson `shouldBe` person1

            receivedReq <- takeMVar receivedReqMVar
            receivedBody <- takeMVar receivedBodyMVar
            requestMethod receivedReq `shouldBe` "PUT"
            rawPathInfo receivedReq `shouldBe` "/v1/people"
            (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
            (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"
            decode receivedBody `shouldBe` Just updatePerson

            stopMockServer svr

        it "updateEntityEither for a person sends JSON encoded UpdatePerson as its body of PUT request" $ do
            receivedReqMVar <- newEmptyMVar
            receivedBodyMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                strictRequestBody req >>= putMVar receivedBodyMVar
                simpleApp personJson1 req respond

            (Right resPerson) <- getResponseBody <$> updateEntityEither dummyAuth mockBaseRequest updatePerson
            resPerson `shouldBe` person1

            receivedReq <- takeMVar receivedReqMVar
            receivedBody <- takeMVar receivedBodyMVar
            requestMethod receivedReq `shouldBe` "PUT"
            rawPathInfo receivedReq `shouldBe` "/v1/people"
            (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
            (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"
            decode receivedBody `shouldBe` Just updatePerson

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
            newTeam = CreateTeam $ TeamName "Build Squad"
            updateTeam = UpdateTeam $ TeamName "updatedTeamName"

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

        it "streamTeamList stops pagination at invalid Link Header" $ do
            svr <- startMockServer . invalidPaginationApp $ encode . TeamList <$> teamListList

            res <- runConduit $ streamTeamList dummyAuth mockBaseRequest .| sinkList
            res `shouldBe` concat (take 2 teamListList)

            stopMockServer svr

        it "getDetail for a team returns a Team" $ do
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp teamJson req respond

            resTeam <- getResponseBody <$> getDetail dummyAuth mockBaseRequest (TeamId "testTeamId")
            resTeam `shouldBe` team

            receivedReq <- takeMVar receivedReqMVar
            requestMethod receivedReq `shouldBe` "GET"
            rawPathInfo receivedReq `shouldBe` "/v1/teams/testTeamId"
            (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
            (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"

            stopMockServer svr

        it "getDetailEither for a team returns a (Right Team)" $ do
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp teamJson req respond
            (Right resTeam) <- getResponseBody <$> getDetailEither dummyAuth mockBaseRequest (TeamId "testTeamId")
            resTeam `shouldBe` team

            stopMockServer svr

        it "createEntity for a team sends JSON encoded CreateTeam as its body of POST request" $ do
            receivedReqMVar <- newEmptyMVar
            receivedBodyMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                strictRequestBody req >>= putMVar receivedBodyMVar
                simpleApp teamJson req respond

            resTeam <- getResponseBody <$> createEntity dummyAuth mockBaseRequest newTeam
            resTeam `shouldBe` team

            receivedReq <- takeMVar receivedReqMVar
            receivedBody <- takeMVar receivedBodyMVar
            requestMethod receivedReq `shouldBe` "POST"
            rawPathInfo receivedReq `shouldBe` "/v1/teams"
            (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
            (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"
            decode receivedBody `shouldBe` Just newTeam

            stopMockServer svr

        it "createEntityEither for a team sends JSON encoded CreateTeam as its body of POST request" $ do
            receivedReqMVar <- newEmptyMVar
            receivedBodyMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                strictRequestBody req >>= putMVar receivedBodyMVar
                simpleApp teamJson req respond

            (Right resTeam) <- getResponseBody <$> createEntityEither dummyAuth mockBaseRequest newTeam
            resTeam `shouldBe` team

            receivedReq <- takeMVar receivedReqMVar
            receivedBody <- takeMVar receivedBodyMVar
            requestMethod receivedReq `shouldBe` "POST"
            rawPathInfo receivedReq `shouldBe` "/v1/teams"
            (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
            (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"
            decode receivedBody `shouldBe` Just newTeam

            stopMockServer svr

        it "updateEntity for a team sends JSON encoded UpdateTeam as its body of PUT request" $ do
            receivedReqMVar <- newEmptyMVar
            receivedBodyMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                strictRequestBody req >>= putMVar receivedBodyMVar
                simpleApp teamJson req respond

            resTeam <- getResponseBody <$> updateEntity dummyAuth mockBaseRequest updateTeam
            resTeam `shouldBe` team

            receivedReq <- takeMVar receivedReqMVar
            receivedBody <- takeMVar receivedBodyMVar
            requestMethod receivedReq `shouldBe` "PUT"
            rawPathInfo receivedReq `shouldBe` "/v1/teams"
            (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
            (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"
            decode receivedBody `shouldBe` Just updateTeam

            stopMockServer svr

        it "updateEntityEither for a team sends JSON encoded UpdateTeam as its body of PUT request" $ do
            receivedReqMVar <- newEmptyMVar
            receivedBodyMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                strictRequestBody req >>= putMVar receivedBodyMVar
                simpleApp teamJson req respond

            (Right resTeam) <- getResponseBody <$> updateEntityEither dummyAuth mockBaseRequest updateTeam
            resTeam `shouldBe` team

            receivedReq <- takeMVar receivedReqMVar
            receivedBody <- takeMVar receivedBodyMVar
            requestMethod receivedReq `shouldBe` "PUT"
            rawPathInfo receivedReq `shouldBe` "/v1/teams"
            (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
            (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"
            decode receivedBody `shouldBe` Just updateTeam

            stopMockServer svr

        it "deleteTeam sends DELETE request with teamId on URL path" $ do
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp "" req respond

            deleteTeam dummyAuth mockBaseRequest (TeamId "testTeamId")

            receivedReq <- takeMVar receivedReqMVar
            requestMethod receivedReq `shouldBe` "DELETE"
            rawPathInfo receivedReq `shouldBe` "/v1/teams/testTeamId"
            (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
            (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"

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
            defFilter = defaultTeamMembershipFilter $ TeamId "dummyTeamId"
            newTeamMembership = CreateTeamMembership { createTeamMembershipTeamId       = TeamId "targetTeam"
                                                     , createTeamMembershipPersonId     = Just $ PersonId "addedPerson"
                                                     , createTeamMembershipPersonEmail  = Just $ Email "added@example.com"
                                                     , createTeamMembershipIsModerator  = Just True
                                                     }
            updateTeamMembership = UpdateTeamMembership False

        it "streamTeamMembershipList streams TeamMembership" $ do
            let testData = teamMembershipList ['Z']
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp (encode (TeamMembershipList testData)) req respond

            res <- runConduit $ streamEntityWithFilter dummyAuth mockBaseRequest defFilter .| sinkList
            res `shouldBe` testData

            receivedReq <- takeMVar receivedReqMVar
            rawPathInfo receivedReq `shouldBe` "/v1/team/memberships"
            queryString receivedReq `shouldBe` [ ("teamId", Just "dummyTeamId") ]

            stopMockServer svr

        it "streamMembershipList passes query strings build from TeamMembershipFilter to server" $ do
            let testData = teamMembershipList ['Z']
                teamMembershipFilter = TeamMembershipFilter $ TeamId "DummyTeamId"

            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp (encode (TeamMembershipList testData)) req respond

            res <- runConduit $ streamEntityWithFilter dummyAuth mockBaseRequest teamMembershipFilter .| sinkList
            res `shouldBe` testData

            receivedReq <- takeMVar receivedReqMVar
            rawPathInfo receivedReq `shouldBe` "/v1/team/memberships"
            queryString receivedReq `shouldBe` [ ("teamId", Just "DummyTeamId") ]

            stopMockServer svr

        it "streamTeamMembershipList streams TeamMembership with automatic pagination" $ do
            svr <- startMockServer . paginationApp $ encode . TeamMembershipList <$> teamMembershipListList

            res <- runConduit $ streamEntityWithFilter dummyAuth mockBaseRequest defFilter .| sinkList
            res `shouldBe` concat teamMembershipListList

            stopMockServer svr

        it "streamTeamMembershipList stops pagination at invalid Link Header" $ do
            svr <- startMockServer . invalidPaginationApp $ encode . TeamMembershipList <$> teamMembershipListList

            res <- runConduit $ streamEntityWithFilter dummyAuth mockBaseRequest defFilter .| sinkList
            res `shouldBe` concat (take 2 teamMembershipListList)

            stopMockServer svr

        it "getDetail for a team membership returns a TeamMembership" $ do
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp teamMembershipJson req respond

            resTeamMembership <- getResponseBody <$> getDetail dummyAuth mockBaseRequest (TeamMembershipId "testTeamMembershipId")
            resTeamMembership `shouldBe` teamMembership

            receivedReq <- takeMVar receivedReqMVar
            requestMethod receivedReq `shouldBe` "GET"
            rawPathInfo receivedReq `shouldBe` "/v1/team/memberships/testTeamMembershipId"
            (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
            (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"

            stopMockServer svr

        it "getDetailEither for a team membership  returns a (Right TeamMembership)" $ do
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp teamMembershipJson req respond
            (Right resTeamMembership) <- getResponseBody <$> getDetailEither dummyAuth mockBaseRequest (TeamMembershipId "testTeamMembershipId")
            resTeamMembership `shouldBe` teamMembership

            stopMockServer svr

        it "createEntity for a team membership sends JSON encoded CreateTeamMembership as its body of POST request" $ do
            receivedReqMVar <- newEmptyMVar
            receivedBodyMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                strictRequestBody req >>= putMVar receivedBodyMVar
                simpleApp teamMembershipJson req respond

            resTeamMembership <- getResponseBody <$> createEntity dummyAuth mockBaseRequest newTeamMembership
            resTeamMembership `shouldBe` teamMembership

            receivedReq <- takeMVar receivedReqMVar
            receivedBody <- takeMVar receivedBodyMVar
            requestMethod receivedReq `shouldBe` "POST"
            rawPathInfo receivedReq `shouldBe` "/v1/team/memberships"
            (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
            (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"
            decode receivedBody `shouldBe` Just newTeamMembership

            stopMockServer svr

        it "createEntityEither for a team membership sends JSON encoded CreateTeamMembership as its body of POST request" $ do
            receivedReqMVar <- newEmptyMVar
            receivedBodyMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                strictRequestBody req >>= putMVar receivedBodyMVar
                simpleApp teamMembershipJson req respond

            (Right resTeamMembership) <- getResponseBody <$> createEntityEither dummyAuth mockBaseRequest newTeamMembership
            resTeamMembership `shouldBe` teamMembership

            receivedReq <- takeMVar receivedReqMVar
            receivedBody <- takeMVar receivedBodyMVar
            requestMethod receivedReq `shouldBe` "POST"
            rawPathInfo receivedReq `shouldBe` "/v1/team/memberships"
            (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
            (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"
            decode receivedBody `shouldBe` Just newTeamMembership

            stopMockServer svr

        it "updateEntity for a teamMembership sends JSON encoded UpdateTeamMembership as its body of PUT request" $ do
            receivedReqMVar <- newEmptyMVar
            receivedBodyMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                strictRequestBody req >>= putMVar receivedBodyMVar
                simpleApp teamMembershipJson req respond

            resTeamMembership <- getResponseBody <$> updateEntity dummyAuth mockBaseRequest updateTeamMembership
            resTeamMembership `shouldBe` teamMembership

            receivedReq <- takeMVar receivedReqMVar
            receivedBody <- takeMVar receivedBodyMVar
            requestMethod receivedReq `shouldBe` "PUT"
            rawPathInfo receivedReq `shouldBe` "/v1/team/memberships"
            (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
            (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"
            decode receivedBody `shouldBe` Just updateTeamMembership

            stopMockServer svr

        it "updateEntityEither for a teamMembership sends JSON encoded UpdateTeamMembership as its body of PUT request" $ do
            receivedReqMVar <- newEmptyMVar
            receivedBodyMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                strictRequestBody req >>= putMVar receivedBodyMVar
                simpleApp teamMembershipJson req respond

            (Right resTeamMembership) <- getResponseBody <$> updateEntityEither dummyAuth mockBaseRequest updateTeamMembership
            resTeamMembership `shouldBe` teamMembership

            receivedReq <- takeMVar receivedReqMVar
            receivedBody <- takeMVar receivedBodyMVar
            requestMethod receivedReq `shouldBe` "PUT"
            rawPathInfo receivedReq `shouldBe` "/v1/team/memberships"
            (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
            (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"
            decode receivedBody `shouldBe` Just updateTeamMembership

            stopMockServer svr

        it "deleteTeamMembership sends DELETE request with teamMembershipId on URL path" $ do
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp "" req respond

            deleteTeamMembership dummyAuth mockBaseRequest (TeamMembershipId "testTeamMembershipId")

            receivedReq <- takeMVar receivedReqMVar
            requestMethod receivedReq `shouldBe` "DELETE"
            rawPathInfo receivedReq `shouldBe` "/v1/team/memberships/testTeamMembershipId"
            (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
            (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"

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
            newRoom = CreateRoom (RoomTitle "New Room") (Just $ TeamId "belongingTeam")
            updateRoom = UpdateRoom $ RoomTitle "updatedRoomTitle"

        it "streamRoomList streams Room" $ do
            let testData = roomList ['Z']
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp (encode (RoomList testData)) req respond

            res <- runConduit $ streamEntityWithFilter dummyAuth mockBaseRequest (def :: RoomFilter) .| sinkList
            res `shouldBe` testData

            receivedReq <- takeMVar receivedReqMVar
            rawPathInfo receivedReq `shouldBe` "/v1/rooms"
            queryString receivedReq `shouldBe` []

            stopMockServer svr

        it "streamRoomList passes query strings build from RoomFilter to server" $ do
            let testData = roomList ['Z']
                roomFilter = RoomFilter (Just $ TeamId "dummyTeamId")
                                        (Just RoomTypeGroup)
                                        (Just RoomFilterSortByLastActivity)

            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp (encode (RoomList testData)) req respond

            res <- runConduit $ streamEntityWithFilter dummyAuth mockBaseRequest roomFilter .| sinkList
            res `shouldBe` testData

            receivedReq <- takeMVar receivedReqMVar
            rawPathInfo receivedReq `shouldBe` "/v1/rooms"
            (sort . queryString) receivedReq `shouldBe` sort [ ("type", Just "group")
                                                             , ("sortBy", Just "lastactivity")
                                                             , ("teamId", Just "dummyTeamId") ]

            stopMockServer svr

        it "streamRoomList streams Room with automatic pagination" $ do
            svr <- startMockServer . paginationApp $ encode . RoomList <$> roomListList

            res <- runConduit $ streamEntityWithFilter dummyAuth mockBaseRequest (def :: RoomFilter) .| sinkList
            res `shouldBe` concat roomListList

            stopMockServer svr

        it "streamRoomList stops pagination at invalid Link Header" $ do
            svr <- startMockServer . invalidPaginationApp $ encode . RoomList <$> roomListList

            res <- runConduit $ streamEntityWithFilter dummyAuth mockBaseRequest (def :: RoomFilter) .| sinkList
            res `shouldBe` concat (take 2 roomListList)

            stopMockServer svr

        it "getDetail for a room returns a Room" $ do
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp roomJson req respond

            resRoom <- getResponseBody <$> getDetail dummyAuth mockBaseRequest (RoomId "testRoomId")
            resRoom `shouldBe` room

            receivedReq <- takeMVar receivedReqMVar
            requestMethod receivedReq `shouldBe` "GET"
            rawPathInfo receivedReq `shouldBe` "/v1/rooms/testRoomId"
            (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
            (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"

            stopMockServer svr

        it "getDetailEither for a room returns a (Right Room)" $ do
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp roomJson req respond
            (Right resRoom) <- getResponseBody <$> getDetailEither dummyAuth mockBaseRequest (RoomId "testRoomId")
            resRoom `shouldBe` room

            stopMockServer svr

        it "createEntity for a room sends JSON encoded CreateRoom as its body of POST request" $ do
            receivedReqMVar <- newEmptyMVar
            receivedBodyMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                strictRequestBody req >>= putMVar receivedBodyMVar
                simpleApp roomJson req respond

            resRoom <- getResponseBody <$> createEntity dummyAuth mockBaseRequest newRoom
            resRoom `shouldBe` room

            receivedReq <- takeMVar receivedReqMVar
            receivedBody <- takeMVar receivedBodyMVar
            requestMethod receivedReq `shouldBe` "POST"
            rawPathInfo receivedReq `shouldBe` "/v1/rooms"
            (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
            (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"
            decode receivedBody `shouldBe` Just newRoom

            stopMockServer svr

        it "createEntithEither for a room sends JSON encoded CreateRoom as its body of POST request" $ do
            receivedReqMVar <- newEmptyMVar
            receivedBodyMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                strictRequestBody req >>= putMVar receivedBodyMVar
                simpleApp roomJson req respond

            (Right resRoom) <- getResponseBody <$> createEntityEither dummyAuth mockBaseRequest newRoom
            resRoom `shouldBe` room

            receivedReq <- takeMVar receivedReqMVar
            receivedBody <- takeMVar receivedBodyMVar
            requestMethod receivedReq `shouldBe` "POST"
            rawPathInfo receivedReq `shouldBe` "/v1/rooms"
            (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
            (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"
            decode receivedBody `shouldBe` Just newRoom

            stopMockServer svr

        it "updateEntity for a room sends JSON encoded UpdateRoom as its body of PUT request" $ do
            receivedReqMVar <- newEmptyMVar
            receivedBodyMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                strictRequestBody req >>= putMVar receivedBodyMVar
                simpleApp roomJson req respond

            resRoom <- getResponseBody <$> updateEntity dummyAuth mockBaseRequest updateRoom
            resRoom `shouldBe` room

            receivedReq <- takeMVar receivedReqMVar
            receivedBody <- takeMVar receivedBodyMVar
            requestMethod receivedReq `shouldBe` "PUT"
            rawPathInfo receivedReq `shouldBe` "/v1/rooms"
            (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
            (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"
            decode receivedBody `shouldBe` Just updateRoom

            stopMockServer svr

        it "updateEntityEither for a room sends JSON encoded UpdateRoom as its body of PUT request" $ do
            receivedReqMVar <- newEmptyMVar
            receivedBodyMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                strictRequestBody req >>= putMVar receivedBodyMVar
                simpleApp roomJson req respond

            (Right resRoom) <- getResponseBody <$> updateEntityEither dummyAuth mockBaseRequest updateRoom
            resRoom `shouldBe` room

            receivedReq <- takeMVar receivedReqMVar
            receivedBody <- takeMVar receivedBodyMVar
            requestMethod receivedReq `shouldBe` "PUT"
            rawPathInfo receivedReq `shouldBe` "/v1/rooms"
            (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
            (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"
            decode receivedBody `shouldBe` Just updateRoom

            stopMockServer svr

        it "deleteRoom sends DELETE request with roomId on URL path" $ do
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp "" req respond

            deleteRoom dummyAuth mockBaseRequest (RoomId "testRoomId")

            receivedReq <- takeMVar receivedReqMVar
            requestMethod receivedReq `shouldBe` "DELETE"
            rawPathInfo receivedReq `shouldBe` "/v1/rooms/testRoomId"
            (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
            (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"

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
            newMembership = CreateMembership { createMembershipRoomId       = RoomId "Y2lzY29zcGFyazovL3VzL1JPT00vYmJjZWIxYWQtNDNmMS0zYjU4LTkxNDctZjE0YmIwYzRkMTU0"
                                             , createMembershipPersonId     = Just $ PersonId "Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mNWIzNjE4Ny1jOGRkLTQ3MjctOGIyZi1mOWM0NDdmMjkwNDY"
                                             , createMembershipPersonEmail  = Just $ Email "john.andersen@example.com"
                                             , createMembershipIsModerator  = Just $ True
                                             }
            updateMembership = UpdateMembership False

        it "streamMembershipList streams Membership" $ do
            let testData = membershipList ['Z']
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp (encode (MembershipList testData)) req respond

            res <- runConduit $ streamEntityWithFilter dummyAuth mockBaseRequest (def :: MembershipFilter) .| sinkList
            res `shouldBe` testData

            receivedReq <- takeMVar receivedReqMVar
            rawPathInfo receivedReq `shouldBe` "/v1/memberships"
            queryString receivedReq `shouldBe` []

            stopMockServer svr

        it "streamMembershipList passes query strings build from MembershipFilter to server" $ do
            let testData = membershipList ['Z']
                membershipFilter = MembershipFilter (Just $ RoomId "dummyRoomId")
                                                    (Just $ PersonId "personIdFilter")
                                                    (Just $ Email "personEmailFilter")

            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp (encode (MembershipList testData)) req respond

            res <- runConduit $ streamEntityWithFilter dummyAuth mockBaseRequest membershipFilter .| sinkList
            res `shouldBe` testData

            receivedReq <- takeMVar receivedReqMVar
            rawPathInfo receivedReq `shouldBe` "/v1/memberships"
            (sort . queryString) receivedReq `shouldBe` sort [ ("personId", Just "personIdFilter")
                                                             , ("personEmail", Just "personEmailFilter")
                                                             , ("roomId", Just "dummyRoomId") ]

            stopMockServer svr

        it "streamMembershipList streams Membership with automatic pagination" $ do
            svr <- startMockServer . paginationApp $ encode . MembershipList <$> membershipListList

            res <- runConduit $ streamEntityWithFilter dummyAuth mockBaseRequest (def :: MembershipFilter) .| sinkList
            res `shouldBe` concat membershipListList

            stopMockServer svr

        it "streamMembershipList stops pagination at invalid Link Header" $ do
            svr <- startMockServer . invalidPaginationApp $ encode . MembershipList <$> membershipListList

            res <- runConduit $ streamEntityWithFilter dummyAuth mockBaseRequest (def :: MembershipFilter) .| sinkList
            res `shouldBe` concat (take 2 membershipListList)

            stopMockServer svr

        it "getDetail for a menbership returns a Membership" $ do
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp membershipJson req respond

            resMembership <- getResponseBody <$> getDetail dummyAuth mockBaseRequest (MembershipId "testMembershipId")
            resMembership `shouldBe` membership

            receivedReq <- takeMVar receivedReqMVar
            requestMethod receivedReq `shouldBe` "GET"
            rawPathInfo receivedReq `shouldBe` "/v1/memberships/testMembershipId"
            (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
            (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"

            stopMockServer svr

        it "getDetailEither for a membership returns a (Right Membership)" $ do
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp membershipJson req respond
            (Right resMembership) <- getResponseBody <$> getDetailEither dummyAuth mockBaseRequest (MembershipId "testMembershipId")
            resMembership `shouldBe` membership

            stopMockServer svr

        it "createEntity for a membership sends JSON encoded CreateMembership as its body of POST request" $ do
            receivedReqMVar <- newEmptyMVar
            receivedBodyMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                strictRequestBody req >>= putMVar receivedBodyMVar
                simpleApp membershipJson req respond

            resMembership <- getResponseBody <$> createEntity dummyAuth mockBaseRequest newMembership
            resMembership `shouldBe` membership

            receivedReq <- takeMVar receivedReqMVar
            receivedBody <- takeMVar receivedBodyMVar
            requestMethod receivedReq `shouldBe` "POST"
            rawPathInfo receivedReq `shouldBe` "/v1/memberships"
            (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
            (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"
            decode receivedBody `shouldBe` Just newMembership

            stopMockServer svr

        it "createEntithEither for a membership sends JSON encoded CreateMembership as its body of POST request" $ do
            receivedReqMVar <- newEmptyMVar
            receivedBodyMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                strictRequestBody req >>= putMVar receivedBodyMVar
                simpleApp membershipJson req respond

            (Right resMembership) <- getResponseBody <$> createEntityEither dummyAuth mockBaseRequest newMembership
            resMembership `shouldBe` membership

            receivedReq <- takeMVar receivedReqMVar
            receivedBody <- takeMVar receivedBodyMVar
            requestMethod receivedReq `shouldBe` "POST"
            rawPathInfo receivedReq `shouldBe` "/v1/memberships"
            (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
            (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"
            decode receivedBody `shouldBe` Just newMembership

            stopMockServer svr

        it "updateEntity for a membership sends JSON encoded UpdateMembership as its body of PUT request" $ do
            receivedReqMVar <- newEmptyMVar
            receivedBodyMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                strictRequestBody req >>= putMVar receivedBodyMVar
                simpleApp membershipJson req respond

            resMembership <- getResponseBody <$> updateEntity dummyAuth mockBaseRequest updateMembership
            resMembership `shouldBe` membership

            receivedReq <- takeMVar receivedReqMVar
            receivedBody <- takeMVar receivedBodyMVar
            requestMethod receivedReq `shouldBe` "PUT"
            rawPathInfo receivedReq `shouldBe` "/v1/memberships"
            (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
            (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"
            decode receivedBody `shouldBe` Just updateMembership

            stopMockServer svr

        it "updateEntityEither for a membership sends JSON encoded UpdateMembership as its body of PUT request" $ do
            receivedReqMVar <- newEmptyMVar
            receivedBodyMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                strictRequestBody req >>= putMVar receivedBodyMVar
                simpleApp membershipJson req respond

            (Right resMembership) <- getResponseBody <$> updateEntityEither dummyAuth mockBaseRequest updateMembership
            resMembership `shouldBe` membership

            receivedReq <- takeMVar receivedReqMVar
            receivedBody <- takeMVar receivedBodyMVar
            requestMethod receivedReq `shouldBe` "PUT"
            rawPathInfo receivedReq `shouldBe` "/v1/memberships"
            (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
            (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"
            decode receivedBody `shouldBe` Just updateMembership

            stopMockServer svr

        it "deleteMembership sends DELETE request with membershipId on URL path" $ do
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp "" req respond

            deleteMembership dummyAuth mockBaseRequest (MembershipId "testMembershipId")

            receivedReq <- takeMVar receivedReqMVar
            requestMethod receivedReq `shouldBe` "DELETE"
            rawPathInfo receivedReq `shouldBe` "/v1/memberships/testMembershipId"
            (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
            (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"

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
            defFilter = defaultMessageFilter $ RoomId "dummyRoomId"
            newMessage = CreateMessage { createMessageRoomId        = Just $ RoomId "Y2lzY29zcGFyazovL3VzL1JPT00vYmJjZWIxYWQtNDNmMS0zYjU4LTkxNDctZjE0YmIwYzRkMTU0"
                                       , createMessageToPersonId    = Just $ PersonId "Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mMDZkNzFhNS0wODMzLTRmYTUtYTcyYS1jYzg5YjI1ZWVlMmX"
                                       , createMessageToPersonEmail = Just $ Email "julie@example.com"
                                       , createMessageText          = Just $ MessageText "PROJECT UPDATE - A new project plan has been published on Box: http://box.com/s/lf5vj. The PM for this project is Mike C. and the Engineering Manager is Jane W."
                                       , createMessageMarkdown      = Just $ MessageMarkdown "**PROJECT UPDATE** A new project plan has been published [on Box](http://box.com/s/lf5vj). The PM for this project is <@personEmail:mike@example.com> and the Engineering Manager is <@personEmail:jane@example.com>."
                                       , createMessageFiles         = Just $ [ FileUrl "http://www.example.com/images/media.png" ]
                                       }

        it "streamMessageList streams Message" $ do
            let testData = messageList ['Z']
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp (encode (MessageList testData)) req respond

            res <- runConduit $ streamEntityWithFilter dummyAuth mockBaseRequest defFilter .| sinkList
            res `shouldBe` testData

            receivedReq <- takeMVar receivedReqMVar
            rawPathInfo receivedReq `shouldBe` "/v1/messages"
            queryString receivedReq `shouldBe` [ ("roomId", Just "dummyRoomId") ]

            stopMockServer svr

        it "streamMessageList passes query strings build from MessageFilter to server" $ do
            let testData = messageList ['Z']
                messageFilter = MessageFilter (RoomId "dummyRoomId")
                                              (Just $ MentionedPeopleMe)
                                              (Just $ Timestamp "beforeFilter")
                                              (Just $ MessageId "beforeMessageFilter")

            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp (encode (MessageList testData)) req respond

            res <- runConduit $ streamEntityWithFilter dummyAuth mockBaseRequest messageFilter .| sinkList
            res `shouldBe` testData

            receivedReq <- takeMVar receivedReqMVar
            rawPathInfo receivedReq `shouldBe` "/v1/messages"
            (sort . queryString) receivedReq `shouldBe` sort [ ("roomId", Just "dummyRoomId")
                                                             , ("mentionedPeople", Just "me")
                                                             , ("beforeMessage", Just "beforeMessageFilter")
                                                             , ("before", Just "beforeFilter") ]

            stopMockServer svr

        it "streamMessageList streams Message with automatic pagination" $ do
            svr <- startMockServer . paginationApp $ encode . MessageList <$> messageListList

            res <- runConduit $ streamEntityWithFilter dummyAuth mockBaseRequest defFilter .| sinkList
            res `shouldBe` concat messageListList

            stopMockServer svr

        it "streamMessageList stops pagination at invalid Link Header" $ do
            svr <- startMockServer . invalidPaginationApp $ encode . MessageList <$> messageListList

            res <- runConduit $ streamEntityWithFilter dummyAuth mockBaseRequest defFilter .| sinkList
            res `shouldBe` concat (take 2 messageListList)

            stopMockServer svr

        it "getDetail for a message returns a Message" $ do
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp messageJson req respond

            resMessage <- getResponseBody <$> getDetail dummyAuth mockBaseRequest (MessageId "testMessageId")
            resMessage `shouldBe` message

            receivedReq <- takeMVar receivedReqMVar
            requestMethod receivedReq `shouldBe` "GET"
            rawPathInfo receivedReq `shouldBe` "/v1/messages/testMessageId"
            (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
            (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"

            stopMockServer svr

        it "getDetailEither for a message returns a (Right Message)" $ do
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp messageJson req respond
            (Right resMessage) <- getResponseBody <$> getDetailEither dummyAuth mockBaseRequest (MessageId "testMessageId")
            resMessage `shouldBe` message

            stopMockServer svr

        it "createEntity for a message sends JSON encoded CreateMessage as its body of POST request" $ do
            receivedReqMVar <- newEmptyMVar
            receivedBodyMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                strictRequestBody req >>= putMVar receivedBodyMVar
                simpleApp messageJson req respond

            resMessage <- getResponseBody <$> createEntity dummyAuth mockBaseRequest newMessage
            resMessage `shouldBe` message

            receivedReq <- takeMVar receivedReqMVar
            receivedBody <- takeMVar receivedBodyMVar
            requestMethod receivedReq `shouldBe` "POST"
            rawPathInfo receivedReq `shouldBe` "/v1/messages"
            (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
            (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"
            decode receivedBody `shouldBe` Just newMessage

            stopMockServer svr

        it "createEntithEither for a message sends JSON encoded CreateMessage as its body of POST request" $ do
            receivedReqMVar <- newEmptyMVar
            receivedBodyMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                strictRequestBody req >>= putMVar receivedBodyMVar
                simpleApp messageJson req respond

            (Right resMessage) <- getResponseBody <$> createEntityEither dummyAuth mockBaseRequest newMessage
            resMessage `shouldBe` message

            receivedReq <- takeMVar receivedReqMVar
            receivedBody <- takeMVar receivedBodyMVar
            requestMethod receivedReq `shouldBe` "POST"
            rawPathInfo receivedReq `shouldBe` "/v1/messages"
            (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
            (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"
            decode receivedBody `shouldBe` Just newMessage

            stopMockServer svr

        it "deleteMessage sends DELETE request with messageId on URL path" $ do
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp "" req respond

            deleteMessage dummyAuth mockBaseRequest (MessageId "testMessageId")

            receivedReq <- takeMVar receivedReqMVar
            requestMethod receivedReq `shouldBe` "DELETE"
            rawPathInfo receivedReq `shouldBe` "/v1/messages/testMessageId"
            (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
            (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"

            stopMockServer svr

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

        it "streamOrganizationList stops pagination at invalid Link Header" $ do
            svr <- startMockServer . invalidPaginationApp $ encode . OrganizationList <$> organizationListList

            res <- runConduit $ streamOrganizationList dummyAuth mockBaseRequest .| sinkList
            res `shouldBe` concat (take 2 organizationListList)

            stopMockServer svr

        it "getDetail for an organization returns a Organization" $ do
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp organizationJson req respond

            resOrganization <- getResponseBody <$> getDetail dummyAuth mockBaseRequest (OrganizationId "testOrganizationId")
            resOrganization `shouldBe` organization

            receivedReq <- takeMVar receivedReqMVar
            requestMethod receivedReq `shouldBe` "GET"
            rawPathInfo receivedReq `shouldBe` "/v1/organizations/testOrganizationId"
            (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
            (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"

            stopMockServer svr

        it "getDetailEither for an organization returns a (Right Organization)" $ do
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp organizationJson req respond
            (Right resOrganization) <- getResponseBody <$> getDetailEither dummyAuth mockBaseRequest (OrganizationId "testOrganizationId")
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

            res <- runConduit $ streamEntityWithFilter dummyAuth mockBaseRequest (def :: LicenseFilter) .| sinkList
            res `shouldBe` testData

            receivedReq <- takeMVar receivedReqMVar
            rawPathInfo receivedReq `shouldBe` "/v1/licenses"
            queryString receivedReq `shouldBe` []

            stopMockServer svr

        it "streamLicenseList passes query strings build from LicenseFilter to server" $ do
            let testData = licenseList ['Z']
                licenseFilter = LicenseFilter (Just $ OrganizationId "orgIdFilter")

            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp (encode (LicenseList testData)) req respond

            res <- runConduit $ streamEntityWithFilter dummyAuth mockBaseRequest licenseFilter .| sinkList
            res `shouldBe` testData

            receivedReq <- takeMVar receivedReqMVar
            rawPathInfo receivedReq `shouldBe` "/v1/licenses"
            queryString receivedReq `shouldBe` [ ("orgId", Just "orgIdFilter") ]

            stopMockServer svr

        it "streamLicenseList streams License with automatic pagination" $ do
            svr <- startMockServer . paginationApp $ encode . LicenseList <$> licenseListList

            res <- runConduit $ streamEntityWithFilter dummyAuth mockBaseRequest (def :: LicenseFilter) .| sinkList
            res `shouldBe` concat licenseListList

            stopMockServer svr

        it "streamLicenseList stops pagination at invalid Link Header" $ do
            svr <- startMockServer . invalidPaginationApp $ encode . LicenseList <$> licenseListList

            res <- runConduit $ streamEntityWithFilter dummyAuth mockBaseRequest (def :: LicenseFilter) .| sinkList
            res `shouldBe` concat (take 2 licenseListList)

            stopMockServer svr

        it "getDetail for a license returns a License" $ do
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp licenseJson req respond

            resLicense <- getResponseBody <$> getDetail dummyAuth mockBaseRequest (LicenseId "testLicenseId")
            resLicense `shouldBe` license

            receivedReq <- takeMVar receivedReqMVar
            requestMethod receivedReq `shouldBe` "GET"
            rawPathInfo receivedReq `shouldBe` "/v1/licenses/testLicenseId"
            (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
            (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"

            stopMockServer svr

        it "getDetailEither for a license returns a (Right License)" $ do
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp licenseJson req respond
            (Right resLicense) <- getResponseBody <$> getDetailEither dummyAuth mockBaseRequest (LicenseId "testLicenseId")
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

        it "streamRoleList stops pagination at invalid Link Header" $ do
            svr <- startMockServer . invalidPaginationApp $ encode . RoleList <$> roleListList

            res <- runConduit $ streamRoleList dummyAuth mockBaseRequest .| sinkList
            res `shouldBe` concat (take 2 roleListList)

            stopMockServer svr

        it "getDetail for a role returns a Role" $ do
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp roleJson req respond

            resRole <- getResponseBody <$> getDetail dummyAuth mockBaseRequest (RoleId "testRoleId")
            resRole `shouldBe` role

            receivedReq <- takeMVar receivedReqMVar
            requestMethod receivedReq `shouldBe` "GET"
            rawPathInfo receivedReq `shouldBe` "/v1/roles/testRoleId"
            (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
            (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"

            stopMockServer svr

        it "getDetailEither for a role returns a (Right Role)" $ do
            receivedReqMVar <- newEmptyMVar

            svr <- startMockServer $ \req respond -> do
                putMVar receivedReqMVar req
                simpleApp roleJson req respond
            (Right resRole) <- getResponseBody <$> getDetailEither dummyAuth mockBaseRequest (RoleId "testRoleId")
            resRole `shouldBe` role

            stopMockServer svr
