--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

{-# LANGUAGE DeriveGeneric #-}

module EagerWords.Server.Domain.PlayDetails (
    PlayDetails(..)
  , encode
  , decode
  ) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BC

import EagerWords.Common.Domain.Piece (Piece)
import EagerWords.Common.Domain.PlayPiece (PlayPiece)

-- | Information about a play and replacements for the played pieces.
data PlayDetails =
  WordPlayDetails {
    playPieces :: [PlayPiece]
  , replacementPieces :: [Piece]
  }
  |
  SwapPlayDetails {
    swapPiece :: Piece
  , replacementPiece :: Piece
  }
  deriving (Eq, Show, Generic)

instance FromJSON PlayDetails
instance ToJSON PlayDetails

encode :: PlayDetails -> String
encode playDetails = BC.unpack $ Aeson.encode playDetails

decode :: String -> Maybe PlayDetails
decode encoded = Aeson.decode $ BC.pack encoded




