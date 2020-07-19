--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module Main where

import System.Exit (die)
import System.Environment (getArgs)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad (when)

import qualified EagerWords.Server.Domain.GameEnv as GameEnv
import EagerWords.Server.Domain.ServerConfig (ServerConfig(ServerConfig))
import qualified EagerWords.Server.Domain.ServerConfig as ServerConfig
import qualified EagerWords.Server.Service.GameTransformerStack as TransformerStack
import qualified EagerWords.Server.Service.GameService as GameService

main :: IO ()

-- TODO. Use getOpt. from System.Console.GetOpt.

main = do
    args <- getArgs
    when (null args) $ print "Beware! No config file supplied as argument - using default config parameters."
    let maybeConfigPath = if null args then Nothing else Just $ head args
    serverConfig <- ServerConfig.getServerConfig maybeConfigPath
    -- TODO. Do not print db password - should not be in server config.
    print $ "server config - " ++ show serverConfig
    let ServerConfig {dbConfig} = serverConfig
    eitherGameEnv <- runExceptT $ GameEnv.mkGameEnv serverConfig
    case eitherGameEnv of
      Left error -> do
        let message = "unable to initialize the application environment - "
        die $ message ++ show error
      Right gameEnv -> do
        let Right gameEnv = eitherGameEnv
        runExceptT $ TransformerStack.runDefault gameEnv $ GameService.prepareDb
        print "OK"

