--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

module EagerWords.Server.Domain.PlayInfo where

-- TODO. Merge the data here into Play.

import EagerWords.Server.Domain.PlayDetails (PlayDetails)
import EagerWords.Common.Domain.PlayerType (PlayerType(..))

-- | Master record of a play.
data PlayInfo = PlayInfo {
    number :: Int
  , turn :: PlayerType
  , details :: PlayDetails
} deriving (Eq, Show)

