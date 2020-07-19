--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module EagerWords.Common.Domain.DeviceType (
    DeviceType (..)
) where

import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)
import Control.DeepSeq (NFData)
import qualified Data.Aeson as Aeson

-- | Types of input devices.
data DeviceType = Mouse | Touch
  deriving (Show, Read, Eq, NFData, Generic)

instance FromJSON DeviceType
instance ToJSON DeviceType






