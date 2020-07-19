
--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

{-# LANGUAGE DeriveGeneric #-}

module Bolour.Util.DeployEnv (
  DeployEnv(Dev, Test, Prod)
)where

import Data.Aeson (FromJSON, ToJSON, toJSON)
import GHC.Generics (Generic)

-- | The deployment environment of the application.
data DeployEnv = Dev | Test | Prod
    deriving (Eq, Show, Read, Generic)

instance FromJSON DeployEnv
instance ToJSON DeployEnv
