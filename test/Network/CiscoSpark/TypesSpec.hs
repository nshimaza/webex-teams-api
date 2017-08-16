{-# LANGUAGE OverloadedStrings #-}

module Network.CiscoSpark.TypesSpec where

import           Data.Aeson               (decode, eitherDecode, encode)
import           Data.Monoid              ((<>))

import           Test.Hspec

import           Network.CiscoSpark.Types

spec :: Spec
spec = do
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
            personJson2 = "{\
                          \  \"id\": \"Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mNWIzNjE4Ny1jOGRkLTQ3MjctOGIyZi1mOWM0NDdmMjkwNDY\",\
                          \  \"emails\": [\
                          \    \"sample@email.com\"\
                          \  ],\
                          \  \"displayName\": \"Taro Yamada\",\
                          \  \"nickName\": \"Taro\",\
                          \  \"firstName\": \"Taro\",\
                          \  \"lastName\": \"Yamada\",\
                          \  \"avatar\": \"https://1efa7a94ed21783e352-c62266528714497a17239ececf39e9e2.ssl.cf1.rackcdn.com/V1~54c844c89e678e5a7b16a306bc2897b9~wx29yGtlTpilEFlYzqPKag==~1600\",\
                          \  \"orgId\": \"Y2lzY29zcGFyazovL3VzL09SR0FOSVpBVElPTi85NmFiYzJhYS0zZGNjLTExZTUtYTE1Mi1mZTM0ODE5Y2RjOWE\",\
                          \  \"created\": \"2012-06-15T20:35:48.682Z\",\
                          \  \"lastActivity\": \"2017-06-30T15:30:20.016Z\",\
                          \  \"status\": \"inactive\",\
                          \  \"type\": \"person\"\
                          \}"
            person2 = Person { personId            = PersonId "Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mNWIzNjE4Ny1jOGRkLTQ3MjctOGIyZi1mOWM0NDdmMjkwNDY"
                             , personEmails        = [Email "sample@email.com" ]
                             , personDisplayName   = DisplayName "Taro Yamada"
                             , personNickName      = Just (NickName "Taro")
                             , personFirstName     = Just (FirstName "Taro")
                             , personLastName      = Just (LastName "Yamada")
                             , personAvatar        = Just (AvatarUrl "https://1efa7a94ed21783e352-c62266528714497a17239ececf39e9e2.ssl.cf1.rackcdn.com/V1~54c844c89e678e5a7b16a306bc2897b9~wx29yGtlTpilEFlYzqPKag==~1600")
                             , personOrgId         = OrganizationId "Y2lzY29zcGFyazovL3VzL09SR0FOSVpBVElPTi85NmFiYzJhYS0zZGNjLTExZTUtYTE1Mi1mZTM0ODE5Y2RjOWE"
                             , personRoles         = Nothing
                             , personLicenses      = Nothing
                             , personCreated       = Timestamp "2012-06-15T20:35:48.682Z"
                             , personTimezone      = Nothing
                             , personLastActivity  = Just (Timestamp "2017-06-30T15:30:20.016Z")
                             , personStatus        = Just PersonStatusInactive
                             , personInvitePending = Nothing
                             , personLoginEnabled  = Nothing
                             , personType          = Just PersonTypePerson
                             }
            personListJson = "{\"items\":[" <> personJson1 <> "," <> personJson2 <> "]}"
            personList = PersonList [ person1, person2 ]

        it "can be unwrapped from PersonList" $ do
            unwrap personList `shouldBe` [ person1, person2 ]

        it "decodes People API response JSON" $ do
            eitherDecode personJson1 `shouldBe` Right person1
            (decode . encode) person1 `shouldBe` Just person1

        it "decodes People API response JSON 2" $ do
            eitherDecode personJson2 `shouldBe` Right person2
            (decode . encode) person2 `shouldBe` Just person2

        it "decodes People list" $ do
            eitherDecode personListJson `shouldBe` Right personList
            (decode . encode) personList `shouldBe` Just personList

        it "encodes CreatePerson to JSON" $ do
            let src = CreatePerson
                    { createPersonEmails      = Just [ Email "johnny.chang@foomail.com", Email "jchang@barmail.com"]
                    , createPersonDisplayName = Just $ DisplayName "John Andersen"
                    , createPersonFirstName   = Just $ FirstName "John"
                    , createPersonLastName    = Just $ LastName "Andersen"
                    , createPersonAvatar      = Just $ AvatarUrl "https://1efa7a94ed21783e352-c62266528714497a17239ececf39e9e2.ssl.cf1.rackcdn.com/V1~54c844c89e678e5a7b16a306bc2897b9~wx29yGtlTpilEFlYzqPKag==~1600"
                    , createPersonOrgId       = Just $ OrganizationId "Y2lzY29zcGFyazovL3VzL09SR0FOSVpBVElPTi85NmFiYzJhYS0zZGNjLTExZTUtYTE1Mi1mZTM0ODE5Y2RjOWE"
                    , createPersonRoles       = Just [ RoleId "Y2lzY29zcGFyazovL3VzL1JPT00vOGNkYzQwYzQtZjA5ZS0zY2JhLThjMjYtZGQwZTcwYWRlY2Iy"
                                                     , RoleId "Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mMDZkNzFhNS0wODMzLTRmYTUtYTcyYS1jYzg5YjI1ZWVlMmX" ]
                    , createPersonLicenses    = Just [ LicenseId "Y2lzY29zcGFyazovL3VzL1JPT00vOGNkYzQwYzQtZjA5ZS0zY2JhLThjMjYtZGQwZTcwYWRlY2Iy"
                                                     , LicenseId "Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mMDZkNzFhNS0wODMzLTRmYTUtYTcyYS1jYzg5YjI1ZWVlMmX" ]
                    }
                dst = "{\
                      \  \"emails\" : [ \"johnny.chang@foomail.com\", \"jchang@barmail.com\" ],\
                      \  \"displayName\" : \"John Andersen\",\
                      \  \"firstName\" : \"John\",\
                      \  \"lastName\" : \"Andersen\",\
                      \  \"avatar\" : \"https://1efa7a94ed21783e352-c62266528714497a17239ececf39e9e2.ssl.cf1.rackcdn.com/V1~54c844c89e678e5a7b16a306bc2897b9~wx29yGtlTpilEFlYzqPKag==~1600\",\
                      \  \"orgId\" : \"Y2lzY29zcGFyazovL3VzL09SR0FOSVpBVElPTi85NmFiYzJhYS0zZGNjLTExZTUtYTE1Mi1mZTM0ODE5Y2RjOWE\",\
                      \  \"roles\" : [ \"Y2lzY29zcGFyazovL3VzL1JPT00vOGNkYzQwYzQtZjA5ZS0zY2JhLThjMjYtZGQwZTcwYWRlY2Iy\", \"Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mMDZkNzFhNS0wODMzLTRmYTUtYTcyYS1jYzg5YjI1ZWVlMmX\" ],\
                      \  \"licenses\" : [ \"Y2lzY29zcGFyazovL3VzL1JPT00vOGNkYzQwYzQtZjA5ZS0zY2JhLThjMjYtZGQwZTcwYWRlY2Iy\", \"Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mMDZkNzFhNS0wODMzLTRmYTUtYTcyYS1jYzg5YjI1ZWVlMmX\" ]\
                      \}"
            eitherDecode dst `shouldBe` Right src
            (decode . encode) src `shouldBe` (decode dst :: Maybe CreatePerson)

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
            teamListJson = "{\"items\":[" <> teamJson <> "]}"
            teamList = TeamList [ team ]

        it "can be unwrapped from TeamList" $ do
            unwrap teamList `shouldBe` [ team ]

        it "decodes Team API response JSON" $ do
            eitherDecode teamJson `shouldBe` Right team
            (decode . encode) team `shouldBe` Just team

        it "decodes Team list" $ do
            eitherDecode teamListJson `shouldBe` Right teamList
            (decode . encode) teamList `shouldBe` Just teamList

        it "encodes CreateTeam to JSON" $ do
            let src = CreateTeam { createTeamName = TeamName "My Team" }
                dst = "{\"name\":\"My Team\"}"
            encode src `shouldBe` dst
            (decode . encode) src `shouldBe` Just src

        it "encodes UpdateTeam to JSON" $ do
            let src = UpdateTeam { updateTeamName = TeamName "New Team Name" }
                dst = "{\"name\":\"New Team Name\"}"
            encode src `shouldBe` dst
            (decode . encode) src `shouldBe` Just src

    describe "TeamMembership" $ do
        let teamMembershipJson = "{\
                                 \  \"id\" : \"Y2lzY29zcGFyazovL3VzL1RFQU1fTUVNQkVSU0hJUC8wZmNmYTJiOC1hZGNjLTQ1ZWEtYTc4Mi1lNDYwNTkyZjgxZWY6MTNlMThmNDAtNDJmYy0xMWU2LWE5ZDgtMjExYTBkYzc5NzY5\",\
                                 \  \"teamId\" : \"Y2lzY29zcGFyazovL3VzL1RFQU0vMTNlMThmNDAtNDJmYy0xMWU2LWE5ZDgtMjExYTBkYzc5NzY5\",\
                                 \  \"personId\" : \"Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mNWIzNjE4Ny1jOGRkLTQ3MjctOGIyZi1mOWM0NDdmMjkwNDY\",\
                                 \  \"personEmail\" : \"johnny.chang@foomail.com\",\
                                 \  \"personDisplayName\" : \"John Andersen\",\
                                 \  \"personOrgId\" : \"Y2lzY29zcGFyazovL3VzL09SR0FOSVpBVElPTi85NmFiYzJhYS0zZGNjLTExZTUtYTE1Mi1mZTM0ODE5Y2RjOWE\",\
                                 \  \"isModerator\" : true,\
                                 \  \"created\" : \"2015-10-18T14:26:16.057Z\"\
                                 \}"
            teamMembership = TeamMembership { teamMembershipId                  = TeamMembershipId "Y2lzY29zcGFyazovL3VzL1RFQU1fTUVNQkVSU0hJUC8wZmNmYTJiOC1hZGNjLTQ1ZWEtYTc4Mi1lNDYwNTkyZjgxZWY6MTNlMThmNDAtNDJmYy0xMWU2LWE5ZDgtMjExYTBkYzc5NzY5"
                                            , teamMembershipTeamId              = TeamId "Y2lzY29zcGFyazovL3VzL1RFQU0vMTNlMThmNDAtNDJmYy0xMWU2LWE5ZDgtMjExYTBkYzc5NzY5"
                                            , teamMembershipPersonId            = PersonId "Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mNWIzNjE4Ny1jOGRkLTQ3MjctOGIyZi1mOWM0NDdmMjkwNDY"
                                            , teamMembershipPersonEmail         = Email "johnny.chang@foomail.com"
                                            , teamMembershipPersonDisplayName   = DisplayName "John Andersen"
                                            , teamMembershipPersonOrgId         = OrganizationId "Y2lzY29zcGFyazovL3VzL09SR0FOSVpBVElPTi85NmFiYzJhYS0zZGNjLTExZTUtYTE1Mi1mZTM0ODE5Y2RjOWE"
                                            , teamMembershipIsModerator         = True
                                            , teamMembershipCreated             = Timestamp "2015-10-18T14:26:16.057Z"
                                            }
            teamMembershipListJson = "{\"items\":[" <> teamMembershipJson <> "]}"
            teamMembershipList = TeamMembershipList [ teamMembership ]

        it "can be unwrapped from TeamMembershipList" $ do
            unwrap teamMembershipList `shouldBe` [ teamMembership ]

        it "decodes Team Membership API response JSON" $ do
            eitherDecode teamMembershipJson `shouldBe` Right teamMembership
            (decode . encode) teamMembership `shouldBe` Just teamMembership

        it "decodes Team Membership list" $ do
            eitherDecode teamMembershipListJson `shouldBe` Right teamMembershipList
            (decode . encode) teamMembershipList `shouldBe` Just teamMembershipList

        it "encodes CreateTeamMembership to JSON" $ do
            let src = CreateTeamMembership
                      { createTeamMembershipTeamId      = TeamId "Y2lzY29zcGFyazovL3VzL1RFQU0vMTNlMThmNDAtNDJmYy0xMWU2LWE5ZDgtMjExYTBkYzc5NzY5"
                      , createTeamMembershipPersonId    = Just $ PersonId "Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mNWIzNjE4Ny1jOGRkLTQ3MjctOGIyZi1mOWM0NDdmMjkwNDY"
                      , createTeamMembershipPersonEmail = Just $ Email "johnny.chang@foomail.com"
                      , createTeamMembershipIsModerator = Just True
                      }
                dst = "{\
                      \  \"teamId\" : \"Y2lzY29zcGFyazovL3VzL1RFQU0vMTNlMThmNDAtNDJmYy0xMWU2LWE5ZDgtMjExYTBkYzc5NzY5\",\
                      \  \"personId\" : \"Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mNWIzNjE4Ny1jOGRkLTQ3MjctOGIyZi1mOWM0NDdmMjkwNDY\",\
                      \  \"personEmail\" : \"johnny.chang@foomail.com\",\
                      \  \"isModerator\" : true\
                      \}"
            eitherDecode dst `shouldBe` Right src
            (decode . encode) src `shouldBe` (decode dst :: Maybe CreateTeamMembership)

        it "encodes UpdateTeamMembership to JSON" $ do
            let src = UpdateTeamMembership { updateTeamMembershipIsModerator = False }
                dst = "{\"isModerator\":false}"
            encode src `shouldBe` dst
            (decode . encode) src `shouldBe` Just src

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
            roomListJson = "{\"items\":[" <> roomJson <> "]}"
            roomList = RoomList [ room ]

        it "can be unwrapped from RoomList" $ do
            unwrap roomList `shouldBe` [ room ]

        it "decodes Room API response JSON" $ do
            eitherDecode roomJson `shouldBe` Right room
            (decode . encode) room `shouldBe` Just room

        it "decodes Room list" $ do
            eitherDecode roomListJson `shouldBe` Right roomList
            (decode . encode) roomList `shouldBe` Just roomList

        it "encodes CreateRoom to JSON" $ do
            let src = CreateRoom { createRoomTitle = RoomTitle "Project Unicorn - Sprint 0"
                                 , createRoomTeamId = Just $ TeamId "Y2lzY29zcGFyazovL3VzL1JPT00vNjRlNDVhZTAtYzQ2Yi0xMWU1LTlkZjktMGQ0MWUzNDIxOTcz"
                                 }
                dst = "{\
                      \  \"title\" : \"Project Unicorn - Sprint 0\",\
                      \  \"teamId\" : \"Y2lzY29zcGFyazovL3VzL1JPT00vNjRlNDVhZTAtYzQ2Yi0xMWU1LTlkZjktMGQ0MWUzNDIxOTcz\"\
                      \}"
            eitherDecode dst `shouldBe` Right src
            (decode . encode) src `shouldBe` (decode dst :: Maybe CreateRoom)

        it "encodes UpdateRoom to JSON" $ do
            let src = UpdateRoom { updateRoomTitle =  RoomTitle "Project Unicorn - Sprint 0" }
                dst = "{\"title\":\"Project Unicorn - Sprint 0\"}"
            encode src `shouldBe` dst
            (decode . encode) src `shouldBe` Just src

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
            membershipListJson = "{\"items\":[" <> membershipJson <> "]}"
            membershipList = MembershipList [ membership ]

        it "can be unwrapped from MembershipList" $ do
            unwrap membershipList `shouldBe` [ membership ]

        it "decodes Team Membership API response JSON" $ do
            eitherDecode membershipJson `shouldBe` Right membership
            (decode . encode) membership `shouldBe` Just membership

        it "decodes Team Membership list" $ do
            eitherDecode membershipListJson `shouldBe` Right membershipList
            (decode . encode) membershipList `shouldBe` Just membershipList

        it "encodes CreateMembership to JSON" $ do
            let src = CreateMembership
                      { createMembershipRoomId      = RoomId "Y2lzY29zcGFyazovL3VzL1JPT00vYmJjZWIxYWQtNDNmMS0zYjU4LTkxNDctZjE0YmIwYzRkMTU0"
                      , createMembershipPersonId    = Just $ PersonId "Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mNWIzNjE4Ny1jOGRkLTQ3MjctOGIyZi1mOWM0NDdmMjkwNDY"
                      , createMembershipPersonEmail = Just $ Email "john.andersen@example.com"
                      , createMembershipIsModerator = Just True
                      }
                dst = "{\
                      \  \"roomId\" : \"Y2lzY29zcGFyazovL3VzL1JPT00vYmJjZWIxYWQtNDNmMS0zYjU4LTkxNDctZjE0YmIwYzRkMTU0\",\
                      \  \"personId\" : \"Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mNWIzNjE4Ny1jOGRkLTQ3MjctOGIyZi1mOWM0NDdmMjkwNDY\",\
                      \  \"personEmail\" : \"john.andersen@example.com\",\
                      \  \"isModerator\" : true\
                      \}"
            eitherDecode dst `shouldBe` Right src
            (decode . encode) src `shouldBe` (decode dst :: Maybe CreateMembership)

        it "encodes UpdateMembership to JSON" $ do
            let src = UpdateMembership { updateMembershipIsModerator = False }
                dst = "{\"isModerator\":false}"
            encode src `shouldBe` dst
            (decode . encode) src `shouldBe` Just src

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
            messageListJson = "{\"items\":[" <> messageJson <> "]}"
            messageList = MessageList [ message ]

        it "can be unwrapped from MessageList" $ do
            unwrap messageList `shouldBe` [ message ]

        it "decodes Team Message API response JSON" $ do
            eitherDecode messageJson `shouldBe` Right message
            (decode . encode) message `shouldBe` Just message

        it "decodes Team Message list" $ do
            eitherDecode messageListJson `shouldBe` Right messageList
            (decode . encode) messageList `shouldBe` Just messageList

        it "encodes CreateMessage to JSON" $ do
            let src = CreateMessage
                          { createMessageRoomId         = Just $ RoomId "Y2lzY29zcGFyazovL3VzL1JPT00vYmJjZWIxYWQtNDNmMS0zYjU4LTkxNDctZjE0YmIwYzRkMTU0"
                          , createMessageToPersonId     = Just $ PersonId "Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mMDZkNzFhNS0wODMzLTRmYTUtYTcyYS1jYzg5YjI1ZWVlMmX"
                          , createMessageToPersonEmail  = Just $ Email "julie@example.com"
                          , createMessageText           = Just $ MessageText "PROJECT UPDATE - A new project plan has been published on Box: http://box.com/s/lf5vj. The PM for this project is Mike C. and the Engineering Manager is Jane W."
                          , createMessageMarkdown       = Just $ MessageMarkdown "**PROJECT UPDATE** A new project plan has been published [on Box](http://box.com/s/lf5vj). The PM for this project is <@personEmail:mike@example.com> and the Engineering Manager is <@personEmail:jane@example.com>."
                          , createMessageFiles          = Just $ [ FileUrl "http://www.example.com/images/media.png" ]
                          }
                dst = "{\
                      \  \"roomId\" : \"Y2lzY29zcGFyazovL3VzL1JPT00vYmJjZWIxYWQtNDNmMS0zYjU4LTkxNDctZjE0YmIwYzRkMTU0\",\
                      \  \"toPersonId\" : \"Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mMDZkNzFhNS0wODMzLTRmYTUtYTcyYS1jYzg5YjI1ZWVlMmX\",\
                      \  \"toPersonEmail\" : \"julie@example.com\",\
                      \  \"text\" : \"PROJECT UPDATE - A new project plan has been published on Box: http://box.com/s/lf5vj. The PM for this project is Mike C. and the Engineering Manager is Jane W.\",\
                      \  \"markdown\" : \"**PROJECT UPDATE** A new project plan has been published [on Box](http://box.com/s/lf5vj). The PM for this project is <@personEmail:mike@example.com> and the Engineering Manager is <@personEmail:jane@example.com>.\",\
                      \  \"files\" : [ \"http://www.example.com/images/media.png\" ]\
                      \}"
            eitherDecode dst `shouldBe` Right src
            (decode . encode) src `shouldBe` (decode dst :: Maybe CreateMessage)

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
            organizationListJson = "{\"items\":[" <> organizationJson <> "]}"
            organizationList = OrganizationList [ organization ]

        it "can be unwrapped from OrganizationList" $ do
            unwrap organizationList `shouldBe` [ organization ]

        it "decodes Team Organization API response JSON" $ do
            eitherDecode organizationJson `shouldBe` Right organization
            (decode . encode) organization `shouldBe` Just organization

        it "decodes Team Organization list" $ do
            eitherDecode organizationListJson `shouldBe` Right organizationList
            (decode . encode) organizationList `shouldBe` Just organizationList

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
            licenseListJson = "{\"items\":[" <> licenseJson <> "]}"
            licenseList = LicenseList [ license ]

        it "can be unwrapped from LicenseList" $ do
            unwrap licenseList `shouldBe` [ license ]

        it "decodes Team License API response JSON" $ do
            eitherDecode licenseJson `shouldBe` Right license
            (decode . encode) license `shouldBe` Just license

        it "decodes Team License list" $ do
            eitherDecode licenseListJson `shouldBe` Right licenseList
            (decode . encode) licenseList `shouldBe` Just licenseList

    describe "Role" $ do
        let roleJson = "{\
                       \  \"id\" : \"OTZhYmMyYWEtM2RjYy0xMWU1LWExNTItZmUzNDgxOWNkYzlh\",\
                       \  \"name\" : \"Full Administrator\"\
                       \}"
            role = Role { roleId    = RoleId "OTZhYmMyYWEtM2RjYy0xMWU1LWExNTItZmUzNDgxOWNkYzlh"
                        , roleName  = RoleName "Full Administrator"
                        }
            roleListJson = "{\"items\":[" <> roleJson <> "]}"
            roleList = RoleList [ role ]

        it "can be unwrapped from RoleList" $ do
            unwrap roleList `shouldBe` [ role ]

        it "decodes Team Role API response JSON" $ do
            eitherDecode roleJson `shouldBe` Right role
            (decode . encode) role `shouldBe` Just role

        it "decodes Team Role list" $ do
            eitherDecode roleListJson `shouldBe` Right roleList
            (decode . encode) roleList `shouldBe` Just roleList
