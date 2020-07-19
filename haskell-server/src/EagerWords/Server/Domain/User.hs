--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

{-# LANGUAGE DeriveGeneric #-}

module EagerWords.Server.Domain.User (
    User(..)
  , encode
  , decode
) where

import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BC

-- | A user.
data User = User {
    userId :: String    -- ^ Unique external id of user.
  , name :: String      -- ^ User's name.
  , email :: String     -- ^ User's email.
}
  deriving (Eq, Show, Generic)

instance FromJSON User
instance ToJSON User

encode :: User -> String
encode player = BC.unpack $ Aeson.encode player

decode :: String -> Maybe User
decode encoded = Aeson.decode $ BC.pack encoded


