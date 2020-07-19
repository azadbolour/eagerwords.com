--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

module EagerWords.Server.Service.TypeDefs where

import Control.Monad.Except (ExceptT)
import EagerWords.Server.Domain.GameError (GameError)

type Result res = ExceptT GameError IO res
type ID = String
type GameId = ID
type PlayerId = ID
type JsonEncoded = String
