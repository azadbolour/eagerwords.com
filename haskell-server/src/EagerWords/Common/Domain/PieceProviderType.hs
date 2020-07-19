--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module EagerWords.Common.Domain.PieceProviderType(
  PieceProviderType(..)
  )
  where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Control.DeepSeq (NFData)

-- | Different types of piece generators.
data PieceProviderType = Random | Cyclic
  deriving (Eq, Show, Generic, NFData)

instance FromJSON PieceProviderType
instance ToJSON PieceProviderType




