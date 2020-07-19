--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module EagerWords.Server.Service.GamePersister (
    GamePersister(..)
  , saveGame
  , clearAllData
) where

import Control.Monad (unless)
import Control.Monad.Trans.Except (throwE)

import EagerWords.Server.Service.TypeDefs (Result)
import EagerWords.Server.Domain.User (User, User(User))
-- import EagerWords.Server.Domain.Player (Player)
import EagerWords.Server.Service.GameData (GameData)
import EagerWords.Server.Domain.GameError (GameError(InternalError), GameError(MissingPlayerError))
-- import EagerWords.Server.Domain.Player (Player, Player(Player))
-- import qualified EagerWords.Server.Domain.Player as Player
import EagerWords.Server.Domain.GameBase (GameBase, GameBase(GameBase))
import qualified EagerWords.Server.Domain.GameBase as GameBase
import EagerWords.Server.Service.GameData (GameData, GameData(GameData))
import qualified EagerWords.Server.Service.GameData as GameData

data GamePersister = GamePersister {
    migrate :: Result ()
  , saveUser :: User -> Result ()
  , findUserByUserId :: String -> Result (Maybe User)
  , clearUsers :: Result ()
  , addGame :: GameData -> Result ()
  , updateGame :: GameData -> Result ()
  , findGameById :: String -> Result (Maybe GameData)
  , deleteGame :: String -> Result ()
  , clearGames :: Result ()
}

saveGame :: GamePersister -> GameData -> Result ()
saveGame persister @ GamePersister {findUserByUserId} gameData @ GameData {base} = do
  let GameBase {gameId, playerId = userId} = base
  maybeUser <- findUserByUserId userId
  case maybeUser of
    Nothing -> throwE $ MissingPlayerError userId -- TODO. Should be MissingPlayerIdError.
    Just _ ->
      addOrUpdateGame persister gameData

addOrUpdateGame :: GamePersister -> GameData -> Result ()
addOrUpdateGame GamePersister {findGameById, addGame, updateGame} gameData @ GameData {base} = do
  let GameBase {gameId} = base
  maybeGame <- findGameById gameId
  case maybeGame of
    Nothing -> addGame gameData
    _ -> updateGame gameData

clearAllData :: GamePersister -> Result ()
clearAllData GamePersister {clearUsers, clearGames} = do
  clearGames
  clearUsers


