--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module EagerWords.Common.Domain.PlayerType (
    PlayerType (..)
  , userIndex, machineIndex
  , playerTypeIndex
) where

import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)
import Control.DeepSeq (NFData)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BC

-- | Types of players.
data PlayerType = User | Machine
  deriving (Show, Read, Eq, NFData, Generic)

instance FromJSON PlayerType
instance ToJSON PlayerType

-- | Index of user player - for indexing into an array of user/machine.
userIndex = 0 :: Int
-- | Index of machine player - for indexing into an array of user/machine.
machineIndex = 1 :: Int

-- | Get the index of a player for indexing into an array of user/machine.
playerTypeIndex :: PlayerType -> Int
playerTypeIndex User = userIndex
playerTypeIndex Machine = machineIndex





