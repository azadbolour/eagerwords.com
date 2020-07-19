--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module EagerWords.Common.Message.MachinePlayResponse (
    MachinePlayResponse(..)
  , tupleToMachinePlayResponse
  ) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Control.DeepSeq (NFData)

import Bolour.Plane.Domain.Point
import EagerWords.Common.Domain.PlayPiece
import EagerWords.Common.Domain.GameMiniState

data MachinePlayResponse = MachinePlayResponse {
    gameMiniState :: GameMiniState
  , playedPieces :: [PlayPiece]
  , deadPoints :: [Point]
}
  deriving (Eq, Show, Generic, NFData)

instance FromJSON MachinePlayResponse
instance ToJSON MachinePlayResponse

tupleToMachinePlayResponse :: (GameMiniState, [PlayPiece], [Point]) -> MachinePlayResponse
tupleToMachinePlayResponse (miniState, playPieces, deadPoints) =
  MachinePlayResponse miniState playPieces deadPoints

