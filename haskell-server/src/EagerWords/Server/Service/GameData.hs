--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module EagerWords.Server.Service.GameData where

import Data.Sequence (Seq)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

import EagerWords.Server.Domain.Play (Play)
import EagerWords.Server.Domain.GameBase (GameBase)

-- | Storable/retrievable data about a game in its entirety.
--   Includes the initial condition (base) and the sequence of successive plays.
data GameData = GameData {
    base :: GameBase
  , plays :: Seq Play
}
  deriving (Eq, Show, Generic)

instance FromJSON GameData
instance ToJSON GameData

