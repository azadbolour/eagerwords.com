
--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module EagerWords.Common.Message.CommitPlayResponse (
    CommitPlayResponse(..)
  , tupleToCommitPlayResponse
  ) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Control.DeepSeq (NFData)

import Bolour.Plane.Domain.Point
import EagerWords.Common.Domain.Piece
import EagerWords.Common.Domain.GameMiniState

data CommitPlayResponse = CommitPlayResponse {
    gameMiniState :: GameMiniState
  , replacementPieces :: [Piece]
  , deadPoints :: [Point]
}
  deriving (Eq, Show, Generic, NFData)

instance FromJSON CommitPlayResponse
instance ToJSON CommitPlayResponse

tupleToCommitPlayResponse :: (GameMiniState, [Piece], [Point]) -> CommitPlayResponse
tupleToCommitPlayResponse (miniState, replacementPieces, deadPoints) =
  CommitPlayResponse miniState replacementPieces deadPoints


