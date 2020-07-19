--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

{-# LANGUAGE DeriveGeneric #-}

module EagerWords.Common.Domain.UserDto (
    UserDto(..)
) where

import GHC.Generics
import Data.Aeson

-- | User data transfer object.
data UserDto = UserDto {
    userId :: String
  , name :: String
  , email :: String
}
  deriving (Eq, Show, Generic)

instance FromJSON UserDto
instance ToJSON UserDto



