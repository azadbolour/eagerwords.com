--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module EagerWords.Common.Domain.GameSettings (
  GameSettings(..)
) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Control.DeepSeq (NFData)
import Bolour.Plane.Domain.Axis (Coordinate)
import EagerWords.Common.Domain.PieceProviderType
import EagerWords.Common.Domain.DeviceType
import EagerWords.Common.Domain.PlayerType (PlayerType)

-- | A user's game settings - properties of games.
--   Note. The Haskell standard for language code uses an underscore to
--   separate the language code itself from the country code, e.g., en_US. Same with Java.
--   That is the standard that has to be used in the server.
--   Browsers, on the other, hand use a dash separator. Beware!
--   For now, we are only supporting the generic language without the country code.
data GameSettings = GameSettings {
    dimension :: Coordinate      -- ^ Height and width of the board.
  , squarePixels :: Int          -- ^ The dimension of the board cell on the UI in pixels.
  , trayCapacity :: Int          -- ^ Number of letters in a user or machine tray.
  , languageCode :: String       -- ^ Language code for the language of the word list to use, e.g. "en".
  , pieceProviderType :: PieceProviderType  -- ^ Determines how to generate pieces for a game.
  , startingPlayer :: Maybe PlayerType      -- ^ Starting player - or if not specified random.
  , preferredDevice :: Maybe DeviceType          -- ^ User's preferred input device.
}
  deriving (Eq, Show, Generic, NFData)

instance FromJSON GameSettings
instance ToJSON GameSettings



