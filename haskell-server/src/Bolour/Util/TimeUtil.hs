--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

module Bolour.Util.TimeUtil(
  nowSecs
) where

import Data.Time
import Data.Time.Clock.POSIX
import Data.Int

nowSecs :: IO Int64
nowSecs = do
  utcNow <- getCurrentTime
  let posixSeconds = floor $ utcTimeToPOSIXSeconds utcNow
  return posixSeconds
