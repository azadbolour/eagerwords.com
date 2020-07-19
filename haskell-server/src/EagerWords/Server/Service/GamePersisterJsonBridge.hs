--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module EagerWords.Server.Service.GamePersisterJsonBridge (
    mkBridge
)
where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Sequence as Sequence
import Bolour.Util.VersionStamped (Version, VersionStamped, VersionStamped(VersionStamped))
import qualified Bolour.Util.VersionStamped as VersionStamped
import EagerWords.Server.Domain.User (User, User(User))
import qualified EagerWords.Server.Domain.User as User
-- import EagerWords.Server.Domain.Player (Player, Player(Player))
import EagerWords.Server.Domain.Play (Play(WordPlay), BasePlay(..))
import qualified EagerWords.Server.Domain.Play as Play
import qualified EagerWords.Common.Domain.PlayerType as Player
import EagerWords.Server.Service.GameData (GameData, GameData(GameData))
import qualified EagerWords.Server.Service.GameData as GameData
import EagerWords.Server.Domain.GameBase (GameBase, GameBase(GameBase))
import qualified EagerWords.Server.Domain.GameBase as GameBase
import EagerWords.Server.Domain.GameError (GameError)
import EagerWords.Server.Service.TypeDefs (Result)
import EagerWords.Server.Service.GamePersister (GamePersister, GamePersister(GamePersister))
import EagerWords.Server.Service.GameJsonPersister (GameJsonPersister, GameJsonPersister(GameJsonPersister))
import qualified EagerWords.Server.Service.GameJsonPersister as GameJsonPersister

migrate :: GameJsonPersister -> Result ()
migrate GameJsonPersister {migrate = delegate} =
  delegate

saveUser :: GameJsonPersister -> Version -> User -> Result ()
saveUser GameJsonPersister {saveUser = delegate} version user @ User {userId, name, email} = do
  let json = VersionStamped.encodeWithVersion version user
  delegate userId name email json

findUserByUserId :: GameJsonPersister -> String -> Result (Maybe User)
findUserByUserId GameJsonPersister {findUserByUserId = delegate} userId = do
  maybeJson <- delegate userId
  return $ maybeJson >>= VersionStamped.decodeAndExtract

clearUsers :: GameJsonPersister -> Result ()
clearUsers GameJsonPersister {clearUsers = delegate} = delegate

addGame :: GameJsonPersister -> Version -> GameData -> Result ()
addGame GameJsonPersister {addGame = delegate} version gameData @ GameData {base} = do
  let GameBase {gameId, playerId} = base
      json = VersionStamped.encodeWithVersion version gameData
  delegate gameId playerId json

updateGame :: GameJsonPersister -> Version -> GameData -> Result ()
updateGame GameJsonPersister {updateGame = delegate} version gameData @ GameData {base, plays} = do
  let GameBase {gameId} = base
      json = VersionStamped.encodeWithVersion version gameData
  delegate gameId json

findGameById :: GameJsonPersister -> String -> Result (Maybe GameData)
findGameById GameJsonPersister {findGameById = delegate} gameUid = do
  maybeJson <- delegate gameUid
  let maybeGameData = maybeJson >>= VersionStamped.decodeAndExtract
  return maybeGameData

deleteGame :: GameJsonPersister -> String -> Result ()
deleteGame GameJsonPersister {deleteGame = delegate} = delegate

clearGames :: GameJsonPersister -> Result ()
clearGames GameJsonPersister {clearGames = delegate} = delegate

mkBridge :: GameJsonPersister -> Version -> GamePersister
mkBridge jsonPersister version =
  GamePersister
    (migrate jsonPersister)
    (saveUser jsonPersister version)
    (findUserByUserId jsonPersister)
    (clearUsers jsonPersister)
    (addGame jsonPersister version)
    (updateGame jsonPersister version)
    (findGameById jsonPersister)
    (deleteGame jsonPersister)
    (clearGames jsonPersister)
