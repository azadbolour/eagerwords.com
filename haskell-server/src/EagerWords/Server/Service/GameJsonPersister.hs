--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module EagerWords.Server.Service.GameJsonPersister (
    GameJsonPersister(..)
  , clearAllData
) where

import EagerWords.Server.Service.TypeDefs

data GameJsonPersister = GameJsonPersister {
    migrate :: Result ()
  , saveUser :: String -> String -> String -> JsonEncoded -> Result ()
  , findUserByUserId :: String -> Result (Maybe JsonEncoded)
  , clearUsers :: Result ()
  , addGame :: GameId -> PlayerId -> JsonEncoded -> Result ()
  , updateGame :: GameId -> JsonEncoded -> Result ()
  , findGameById :: GameId -> Result (Maybe JsonEncoded)
  , deleteGame :: GameId -> Result ()
  , clearGames :: Result ()
}

clearAllData :: GameJsonPersister -> Result ()
clearAllData GameJsonPersister {clearUsers, clearGames} = do
  clearGames
  clearUsers


