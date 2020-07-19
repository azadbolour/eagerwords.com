--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module EagerWords.Common.Message.GetGamesResponse (
  GetGamesResponse(..)
  )
  where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Control.DeepSeq (NFData)

import EagerWords.Common.Domain.GameParams (GameParams)
import EagerWords.Common.Domain.Piece (Piece)
import EagerWords.Common.Domain.PiecePoint (PiecePoint)
import EagerWords.Common.Domain.GameBasicInfo (GameBasicInfo)

-- | Data transfer object for basic information on a set of games.
data GetGamesResponse = GetGamesResponse {
    games :: [GameBasicInfo]              
}
  deriving (Eq, Show, Generic, NFData)

instance FromJSON GetGamesResponse
instance ToJSON GetGamesResponse



