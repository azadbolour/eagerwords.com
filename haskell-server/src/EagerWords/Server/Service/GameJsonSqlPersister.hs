--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

{-|
The data access layer for the board game application.
-}
module EagerWords.Server.Service.GameJsonSqlPersister (
    migrateDb
  , mkPersister
) where

import Data.Maybe (listToMaybe, isJust)

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Except (throwE)

import Database.Esqueleto (
    Entity(..)
  , SqlExpr
  , select
  , from
  , insert
  , update
  , upsert
  , delete
  , where_
  , set
  -- , orderBy
  -- , desc
  , val
  , unValue
  , (==.)
  , (=.)
  , (^.)
  )

import Database.Persist.Sql (
    -- ConnectionPool
  -- , SqlPersistT
  SqlPersistM
  , fromSqlKey
  , Key
  , Update(Update)
  , PersistUpdate(Assign)
  , EntityField
  , PersistField
  , PersistRecordBackend
  -- , runSqlPool
  )
import qualified Database.Persist.Sql as PersistSql (update, (=.))

import Database.Persist.TH
import Bolour.Util.Core (EntityId)
import EagerWords.Server.Domain.GameError(GameError(..))
import qualified EagerWords.Server.Domain.GameEnv as GameEnv(GameEnv(..))

import EagerWords.Server.Service.TypeDefs
import EagerWords.Server.Service.GameJsonPersister (GameJsonPersister, GameJsonPersister(GameJsonPersister))

import Bolour.Util.PersistRunner (ConnectionProvider)
import qualified Bolour.Util.PersistRunner as PersistRunner

{-
  To see generated code:
  stack build --ghc-options="-ddump-splices -dsuppress-all"
  find .stack-work -name \*.dump-splices # It is under the dist directory.
-}

{-
   Note. We are not using Persistent's hidden built-in unique identifiers.
   They are called "id", so that name is taken. 
   Our own unique identifiers are therefore called "uid".
-} 
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
UserRow sql=user
    userId String
    name String
    email String
    json String
    UniqueUserId userId
    deriving Show Eq
GameRow sql=game
    uid String
    playerUid String
    json String
    UniqueGameUid uid
    deriving Show Eq
|]

-- TODO. Set up playerId as foreign key of Game.


userToRow :: String -> String -> String -> JsonEncoded -> UserRow
userToRow = UserRow

gameToRow :: GameId -> String -> String -> GameRow
gameToRow = GameRow

migration = migrateAll -- Generated.

migrateDb :: ConnectionProvider -> IO ()
migrateDb provider =
  PersistRunner.migrateDatabase provider migration

-- TODO. Should just have one migrate!

migrate :: ConnectionProvider -> Result ()
migrate provider =
  liftIO $ migrateDb provider

saveUser :: ConnectionProvider -> String -> String -> String -> JsonEncoded -> Result ()
saveUser provider userId name email json = do
  let row = UserRow userId name email json
  maybe <- liftIO $ PersistRunner.runQuery provider (findUserByUserIdReader1 userId)
  case maybe of
    Nothing -> do
      let row = UserRow userId name email json
      liftIO $ PersistRunner.runQuery provider (insertUserReader row)
      return ()
    Just id -> liftIO $ PersistRunner.runQuery provider (updateUserReader name email json row)

insertUserReader :: UserRow -> SqlPersistM (Key UserRow)
insertUserReader row = insert row

-- Update should be easier. 
-- But had issues with the sample code at https://hackage.haskell.org/package/persistent-2.9.2/docs/Database-Persist-Class.html.
-- Specifically, the update expressions there yield SqlExpr of Update but we need just Update.
updateUserReader :: String -> String -> JsonEncoded -> UserRow -> SqlPersistM ()
updateUserReader name email json row = do
  -- 'Set' should be generalized to any Persistent record type. 
  -- But it needs the constraint 'PersistRecordBackend record backend'.
  -- The backend is ambiguous to the compiler. 
  -- Don't know how to disambiguate backend but keep it generic for any database.
  let set :: PersistField typ => (EntityField UserRow typ) -> typ -> Update UserRow
      set field value = Update field value Assign
      updates :: [Update UserRow]
      updates = [set UserRowName name, set UserRowEmail email, set UserRowJson json]
  _ <- upsert row updates
  return ()

findUserByUserId :: ConnectionProvider -> String -> Result (Maybe JsonEncoded)
findUserByUserId provider userId =
  liftIO $ PersistRunner.runQuery provider (findUserByUserIdReader userId)

findUserByUserIdReader :: String -> SqlPersistM (Maybe JsonEncoded)
findUserByUserIdReader userId = do
  selectedList <- select $
    from $ \user -> do
      where_ (user ^. UserRowUserId ==. val userId)
      return $ user ^. UserRowJson
  let maybeValue = listToMaybe selectedList
  return $ unValue <$> maybeValue

findUserByUserIdReader1 :: String -> SqlPersistM (Maybe UserRowId)
findUserByUserIdReader1 userId = do
  selectedList <- select $
    from $ \user -> do
      where_ (user ^. UserRowUserId ==. val userId)
      return $ user ^. UserRowId
  let maybeValue = listToMaybe selectedList
  return $ unValue <$> maybeValue


-- TODO. Use findPlayerById. Remove duplicate code.
findUserRowIdByUserId :: ConnectionProvider -> String -> Result (Maybe UserRowId)
findUserRowIdByUserId provider userId =
  liftIO $ PersistRunner.runQuery provider (findUserRowIdByUserIdReader userId)

findUserRowIdByUserIdReader :: String -> SqlPersistM (Maybe UserRowId)
findUserRowIdByUserIdReader userId = do
  selectedEntityList <- select $
    from $ \user -> do
      where_ (user ^. UserRowUserId ==. val userId)
      return user
  case selectedEntityList of
    [] -> return Nothing
    Entity k _ : _ -> return $ Just k

clearUsers :: ConnectionProvider -> Result ()
clearUsers provider =
  liftIO $ PersistRunner.runQuery provider clearUsersReader

-- TODO. Generic truncate function. Use variable for Row?

clearUsersReader :: SqlPersistM ()
clearUsersReader =
  delete $
    from $ \(player :: SqlExpr (Entity UserRow)) ->
    return ()

addGame :: ConnectionProvider -> GameId -> PlayerId -> JsonEncoded -> Result ()
addGame provider gameUid playerUid json = do
  maybePlayerRowId <- findUserRowIdByUserId provider playerUid
  case maybePlayerRowId of
    Nothing -> throwE $ MissingPlayerError playerUid -- TODO. Should be MissingPlayerIdError
    Just playerRowId -> do
      let row = GameRow gameUid playerUid json
      liftIO $ PersistRunner.runQuery provider (addGameReader row)
      return ()

-- TODO. Generic insert?
addGameReader :: GameRow -> SqlPersistM EntityId
addGameReader row = fromSqlKey <$> insert row

updateGame :: ConnectionProvider -> GameId -> JsonEncoded -> Result ()
updateGame provider gameUid json = do
  maybeEntity <- liftIO $ PersistRunner.runQuery provider (findGameByIdReader gameUid)
  unless (isJust maybeEntity) $ throwE $ MissingGameError gameUid
  liftIO $ PersistRunner.runQuery provider (updateGameReader gameUid json)

updateGameReader :: GameId -> JsonEncoded -> SqlPersistM ()
updateGameReader gameUid json =
  update $ \game -> do
    set game [ GameRowJson =. val json ]
    where_ (game ^. GameRowUid ==. val gameUid)

findGameById :: ConnectionProvider -> GameId -> Result (Maybe JsonEncoded)
findGameById provider gameUid = do
  maybeEntity <- liftIO $ PersistRunner.runQuery provider (findGameByIdReader gameUid)
  let extractJson entity = case entityVal entity of
                             GameRow _ _ json -> json
  return $ extractJson <$> maybeEntity

findGameByIdReader :: String -> SqlPersistM (Maybe (Entity GameRow))
findGameByIdReader gameUid = do
  selectedList <- select $
    from $ \game -> do
      where_ (game ^. GameRowUid ==. val gameUid)
      return game
  let maybeEntity = listToMaybe selectedList
  return maybeEntity

deleteGame :: ConnectionProvider -> GameId -> Result ()
deleteGame provider gameUid =
  liftIO $ PersistRunner.runQuery provider (deleteGameReader gameUid)

deleteGameReader :: GameId -> SqlPersistM ()
deleteGameReader gameUid =
  delete $
    from $ \(game :: SqlExpr (Entity GameRow)) -> do
    where_ (game ^. GameRowUid ==. val gameUid)
    return ()

clearGames :: ConnectionProvider -> Result ()
clearGames provider =
  liftIO $ PersistRunner.runQuery provider clearGamesReader

clearGamesReader :: SqlPersistM ()
clearGamesReader =
  delete $
    from $ \(game :: SqlExpr (Entity GameRow)) ->
    return ()

mkPersister :: ConnectionProvider -> GameJsonPersister
mkPersister provider =
  GameJsonPersister
    (migrate provider)
    (saveUser provider)
    (findUserByUserId provider)
    (clearUsers provider)
    (addGame provider)
    (updateGame provider)
    (findGameById provider)
    (deleteGame provider)
    (clearGames provider)
