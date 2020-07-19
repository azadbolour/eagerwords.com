--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module EagerWords.Common.Domain.InitPieces (
    InitPieces(..)
)
where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Control.DeepSeq (NFData)

import EagerWords.Common.Domain.Piece (Piece)
import EagerWords.Common.Domain.PiecePoint (PiecePoint)

data InitPieces = InitPieces {
    boardPiecePoints :: [PiecePoint]
  , userPieces :: [Piece]
  , machinePieces :: [Piece]
}
  deriving (Eq, Show, Generic, NFData)

instance FromJSON InitPieces
instance ToJSON InitPieces
