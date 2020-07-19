--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}

module EagerWords.Server.Domain.GameBase (
    GameBase(..)
  , mkInitialBase
)
where

import Data.Int
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Except (MonadError(..))

import qualified Bolour.Util.MiscUtil as Util (mkUuid)
import Bolour.Util.TimeUtil (nowSecs)
import EagerWords.Common.Domain.PieceProviderType (PieceProviderType)
import EagerWords.Common.Domain.GameParams (GameParams, GameParams(GameParams))
import qualified EagerWords.Common.Domain.GameParams as GameParams
import EagerWords.Common.Domain.Piece (Piece)
import EagerWords.Common.Domain.InitPieces (InitPieces, InitPieces(InitPieces))
import qualified EagerWords.Common.Domain.InitPieces as InitPieces
import EagerWords.Common.Domain.InitPieces (InitPieces)
import EagerWords.Common.Domain.GameSettings (GameSettings, GameSettings(GameSettings))
import qualified EagerWords.Common.Domain.GameSettings as GameSettings
import qualified EagerWords.Common.Domain.InitPieces as InitPieces
import EagerWords.Server.Domain.GameError (GameError)
import EagerWords.Server.Domain.Tray (Tray)
import qualified EagerWords.Server.Domain.Tray as Tray
import EagerWords.Server.Domain.PieceProvider (PieceProvider)
import qualified EagerWords.Server.Domain.PieceProvider as PieceProvider

-- Note. Excludes initial board to save space.
-- Initial board is reconstructed by using InitPiece {piecePoints}.

-- | The base state of the game - properties that are conceptually
--   immutable once set, and can be used to reconstitute the initial state
--   of the game.
data GameBase = GameBase {
    gameId :: String
  , gameParams :: GameParams
  , initPieces :: InitPieces
  , playerId :: String
  , firstSecond :: Int64
}
  deriving (Eq, Show, Generic)

instance FromJSON GameBase
instance ToJSON GameBase

mkInitialBase :: GameParams -> InitPieces -> String -> IO GameBase

mkInitialBase gameParams initPieces playerId = do
  let GameParams {settings = GameSettings {dimension, trayCapacity, languageCode}} = gameParams
  gameId <- Util.mkUuid
  posixFirstSecond <- nowSecs
  let base = GameBase gameId gameParams initPieces playerId posixFirstSecond
  return base


