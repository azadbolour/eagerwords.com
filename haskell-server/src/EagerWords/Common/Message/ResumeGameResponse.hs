--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module EagerWords.Common.Message.ResumeGameResponse (
  ResumeGameResponse(..)
  )
  where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Control.DeepSeq (NFData)

import EagerWords.Common.Domain.GameParams (GameParams)
import EagerWords.Common.Domain.Piece (Piece)
import EagerWords.Common.Domain.PiecePoint (PiecePoint)

-- | Data transfer object for a game.
--   The machine tray is excluded -
--   we don't want to reveal the machine's hand to client programs.
data ResumeGameResponse = ResumeGameResponse {
    gameId :: String              -- ^ The unique identifier of the game.
  , gameParams :: GameParams      -- ^ The game parameters.
  , boardPiecePoints :: [PiecePoint]   -- ^ The pieces in play and their positions.
  , trayPieces :: [Piece]         -- ^ The pieces on the user tray.
  , userScore :: Int
  , machineScore :: Int

}
  deriving (Eq, Show, Generic, NFData)

instance FromJSON ResumeGameResponse
instance ToJSON ResumeGameResponse



