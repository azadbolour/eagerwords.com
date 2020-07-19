--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module EagerWords.Common.Domain.GameParams (
  GameParams(..)
) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Control.DeepSeq (NFData)
import Bolour.Plane.Domain.Axis (Coordinate)
import EagerWords.Common.Domain.PieceProviderType
import EagerWords.Common.Domain.GameSettings

data GameParams = GameParams {
    settings :: GameSettings
  , pointValues :: [[Int]]  -- Values assigned to the board point.
}
  deriving (Eq, Show, Generic, NFData)

instance FromJSON GameParams
instance ToJSON GameParams



