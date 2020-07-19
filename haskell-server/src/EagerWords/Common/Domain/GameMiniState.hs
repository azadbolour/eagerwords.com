--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module EagerWords.Common.Domain.GameMiniState (
  GameMiniState(..)
) where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)

-- | Minimal state of the game needed by clients.
data GameMiniState = GameMiniState {
    lastPlayScore :: Int
  , scores :: [Int]
  , noMorePlays :: Bool
}
  deriving (Eq, Show, Generic, NFData)

instance FromJSON GameMiniState
instance ToJSON GameMiniState

