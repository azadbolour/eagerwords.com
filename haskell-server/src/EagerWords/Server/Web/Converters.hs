--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module EagerWords.Server.Web.Converters (
    Converter(..)
  , gameToStartGameResponse
  , userDtoToUser
  )
  where

import Data.Time
import EagerWords.Common.Domain.GameParams (GameParams, GameParams(GameParams))
import qualified EagerWords.Common.Domain.GameParams as GameParams
import EagerWords.Common.Message.StartGameResponse as StartGameResponse
import EagerWords.Server.Domain.Game(Game, Game(Game))
import EagerWords.Server.Domain.User
import EagerWords.Common.Domain.UserDto
import qualified EagerWords.Server.Domain.Game as Game
import EagerWords.Server.Domain.GameBase(GameBase, GameBase(GameBase))
import qualified EagerWords.Server.Domain.GameBase as GameBase
import EagerWords.Server.Domain.Board as Board
import EagerWords.Server.Domain.Tray as Tray
import qualified EagerWords.Common.Domain.PlayerType as Player

import qualified EagerWords.Server.Domain.PieceProvider as PieceProvider
import EagerWords.Server.Domain.PieceProvider
import EagerWords.Common.Domain.PieceProviderType
import qualified EagerWords.Common.Domain.PieceProviderType as PieceProviderType
import EagerWords.Common.Domain.GameSettings (GameSettings, GameSettings(GameSettings))
import qualified EagerWords.Common.Domain.GameSettings as GameSettings

dummyDay :: Day
dummyDay = fromGregorian 2000 1 1

zeroDiffTime :: DiffTime
zeroDiffTime = secondsToDiffTime 0

dummyUTCTime :: UTCTime
dummyUTCTime = UTCTime dummyDay zeroDiffTime

class Converter entity dto where
  toEntity :: dto -> entity
  toDto :: entity -> dto

gameToStartGameResponse Game {gameBase, board, trays, pieceProvider} =
   let GameBase {gameId, gameParams} = gameBase
       GameParams {settings = GameSettings{dimension, languageCode}} = gameParams
       pieceProviderType = PieceProvider.pieceProviderType pieceProvider
       Tray {capacity, pieces = trayPieces} = trays !! Player.userIndex
   in StartGameResponse gameId (Board.getPiecePoints board) trayPieces

userDtoToUser :: UserDto -> User
userDtoToUser UserDto {userId, name, email} = User userId name email
