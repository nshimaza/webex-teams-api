{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

{-|
Module      : Network.WebexTeams.Pipes
Copyright   : (c) Naoto Shimazaki 2018
License     : MIT (see the file LICENSE)

Maintainer  : https://github.com/nshimaza
Stability   : experimental

This module provides Pipes wrapper for Cisco Webex Teams list APIs.

-}
module Network.WebexTeams.Pipes
    (
    -- * Functions
      streamListWithFilter
    , streamTeamList
    , streamOrganizationList
    , streamRoleList
    ) where

import           Control.Monad      (unless)
import           Data.Foldable      (traverse_)
import           Pipes              (MonadIO, Producer', liftIO, yield)

import           Network.WebexTeams hiding (streamOrganizationList,
                                     streamRoleList, streamTeamList)


{-|
    Common worker function for List APIs.
    It accesses List API with given 'Request', unwrap result into list of items, stream them to Pipes pipe
    and finally it automatically accesses next page designated via HTTP Link header if available.
-}
readerToProducer :: MonadIO m => ListReader i -> Producer' i m ()
readerToProducer reader = go
  where
    go = do
        xs <- liftIO reader
        unless (null xs) $ do
            traverse_ yield xs
            go

-- | Get list of entities with query parameter and stream it into Pipes.  It automatically performs pagination.
streamListWithFilter :: (MonadIO m, WebexTeamsFilter filter, WebexTeamsListItem (ToResponse filter))
    => Authorization        -- ^ Authorization string against Webex Teams API.
    -> WebexTeamsRequest    -- ^ Predefined part of 'Request' commonly used for Webex Teams API.
    -> filter               -- ^ Filter criteria of the request.  Type of filter automatically determines
                            --   item type in response.
    -> Producer' (ToResponse filter) m ()
streamListWithFilter auth base param = getListWithFilter auth base param >>= readerToProducer

-- | List of 'Team' and stream it into Pipes.  It automatically performs pagination.
streamTeamList :: MonadIO m => Authorization -> WebexTeamsRequest -> Producer' Team m ()
streamTeamList auth base = getTeamList auth base >>= readerToProducer

-- | Filter list of 'Organization' and stream it into Pipes.  It automatically performs pagination.
streamOrganizationList :: MonadIO m => Authorization -> WebexTeamsRequest -> Producer' Organization m ()
streamOrganizationList auth base = getOrganizationList auth base >>= readerToProducer

-- | List of 'Role' and stream it into Pipes.  It automatically performs pagination.
streamRoleList :: MonadIO m => Authorization -> WebexTeamsRequest -> Producer' Role m ()
streamRoleList auth base = getRoleList auth base >>= readerToProducer
