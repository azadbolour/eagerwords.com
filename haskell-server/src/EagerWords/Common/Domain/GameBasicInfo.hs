--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module EagerWords.Common.Domain.GameBasicInfo (
  GameBasicInfo(..)
) where

import Data.Int
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)

-- | Basic information about a game.
data GameBasicInfo = GameBasicInfo {
    gameId :: String
  , userId :: String
  , firstSecond :: Int64
  , lastSecond :: Int64
  , status :: String
  , userScore :: Int
  , machineScore :: Int
}
  deriving (Eq, Show, Generic, NFData)

instance FromJSON GameBasicInfo
instance ToJSON GameBasicInfo

