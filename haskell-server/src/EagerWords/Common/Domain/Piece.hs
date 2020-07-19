--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- TODO. mkPiece is really deprecated. But let's not clutter the build output for now.
-- {-# DEPRECATED mkPiece "Use appropriate pieceOf function of appropriate PieceProvider." #-}

module EagerWords.Common.Domain.Piece (
    Piece(..)
) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Control.DeepSeq (NFData)

-- | A game piece aka tile.
data Piece = Piece {
    value :: Char,      -- ^ The letter - an upper case alpha character.
    id :: String        -- ^ The unique id of the piece.
}
  deriving (Eq, Show, Generic, NFData)

instance FromJSON Piece
instance ToJSON Piece
