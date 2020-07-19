--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module EagerWords.Server.Domain.GameEnv (
    GameEnv(..)
  , mkGameEnv
  , getDictionaryDir
 )
where

import Control.Monad.Except (ExceptT(ExceptT), MonadError(..), withExceptT)
import Control.Monad.IO.Class (liftIO)
import Bolour.Util.PersistRunner (ConnectionProvider)
import EagerWords.Server.Domain.ServerConfig (ServerConfig, ServerConfig(ServerConfig))
import qualified EagerWords.Server.Domain.ServerConfig as ServerConfig
import EagerWords.Server.Domain.GameCache (GameCache, GameCache(GameCache))
import EagerWords.Server.Domain.GameError (GameError, GameError(InternalError))
import qualified EagerWords.Server.Domain.GameCache as GameCache
import Bolour.Language.Domain.DictionaryCache (DictionaryCache)
import qualified Bolour.Language.Domain.DictionaryCache as DictCache
import qualified Bolour.Language.Domain.DictionaryIO as DictionaryIO
import qualified Bolour.Util.PersistRunner as PersistRunner
import Bolour.Util.MiscUtil (IOEither, IOExceptT)

import qualified Paths_eagerwords as ResourcePaths

data GameEnv = GameEnv {
    serverConfig :: ServerConfig
  , connectionProvider :: ConnectionProvider
  , gameCache :: GameCache
  , dictionaryCache :: DictionaryCache
}

mkGameEnv :: ServerConfig -> IOExceptT GameError GameEnv
mkGameEnv serverConfig = do
    let ServerConfig {maxActiveGames, dictionaryDir = configuredDictionaryDir, languageCodes, dbConfig} = serverConfig
    connectionProvider <- liftIO $ PersistRunner.mkConnectionProvider dbConfig
    gameCache <- liftIO $ GameCache.mkGameCache maxActiveGames
    dictionaryDir <- liftIO $ getDictionaryDir configuredDictionaryDir
    let convertException = withExceptT $ \string -> InternalError string
    dictionaryCache <- convertException $! DictionaryIO.readAllDictionaries dictionaryDir languageCodes ServerConfig.maxDictionaries ServerConfig.dictionaryMaxMaskedLetters
    return $ GameEnv serverConfig connectionProvider gameCache dictionaryCache

getDictionaryDir :: String -> IO String
getDictionaryDir configuredDictionaryDir =
  if null configuredDictionaryDir
      then defaultDictionaryDir
      else return configuredDictionaryDir

defaultDictionaryDir :: IO String
defaultDictionaryDir = do
  dataDir <- ResourcePaths.getDataDir
  return $ dataDir ++ "/data"




