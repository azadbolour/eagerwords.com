
--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module EagerWords.Common.Message.GetUserGamesRequest (
    GetUserGamesRequest(..)
  )
  where

import Data.Int
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Control.DeepSeq (NFData)

import EagerWords.Common.Domain.GameParams (GameParams)
import EagerWords.Common.Domain.Piece (Piece)
import EagerWords.Common.Domain.InitPieces (InitPieces)

data GetUserGamesRequest = GetUserGamesRequest {
    fromEpochSecond :: Int64
  , toEpochSecond :: Int64
  , maxGames :: Int
} deriving (Eq, Show, Generic, NFData)

instance FromJSON GetUserGamesRequest
instance ToJSON GetUserGamesRequest






