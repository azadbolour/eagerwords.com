
--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module EagerWords.Common.Message.StartGameRequest (
    StartGameRequest(..)
  )
  where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Control.DeepSeq (NFData)

import EagerWords.Common.Domain.GameParams (GameParams)
import EagerWords.Common.Domain.Piece (Piece)
import EagerWords.Common.Domain.InitPieces (InitPieces)

data StartGameRequest = StartGameRequest {
    gameParams :: GameParams
  , initPieces :: InitPieces
  , userId :: String
} deriving (Eq, Show, Generic, NFData)

instance FromJSON StartGameRequest
instance ToJSON StartGameRequest






