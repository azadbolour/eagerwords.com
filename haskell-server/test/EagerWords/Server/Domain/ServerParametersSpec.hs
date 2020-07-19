--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module EagerWords.Server.Domain.ServerParametersSpec where

import Test.Hspec

import EagerWords.Server.Domain.ServerConfig (ServerConfig, ServerConfig(ServerConfig))
import Bolour.Util.DeployEnv (DeployEnv(..))
import qualified EagerWords.Server.Domain.ServerConfig as ServerConfig

spec :: Spec
spec = do
  describe "read properties" $ do
    it "read properties" $ do
        serverParameters <- ServerConfig.getServerConfig $ Just "test-data/test-config.yml"
        print serverParameters