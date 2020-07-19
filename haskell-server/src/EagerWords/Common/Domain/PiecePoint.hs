--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}

module EagerWords.Common.Domain.PiecePoint (
    PiecePoint(..)
  , pointLetter
)
where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import EagerWords.Common.Domain.Piece
import qualified EagerWords.Common.Domain.Piece as Piece
import Bolour.Plane.Domain.Point (Point)

-- | A piece located at a point on the plane.
data PiecePoint = PiecePoint {
    piece :: Piece    -- ^ The piece.
  , point :: Point    -- ^ The position of the piece on the plane.
} deriving (Eq, Show, Generic, NFData)

instance FromJSON PiecePoint
instance ToJSON PiecePoint

pointLetter :: PiecePoint -> Char
pointLetter PiecePoint {piece} = Piece.value piece

