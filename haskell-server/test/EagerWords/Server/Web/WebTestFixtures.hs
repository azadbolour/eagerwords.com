--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module EagerWords.Server.Web.WebTestFixtures (
    module EagerWords.Server.Service.BaseServiceFixtures
  , mkGameWeb
  , mkUserWeb
  ) where

import Servant (runHandler)

import Bolour.Util.SpecUtil (satisfiesRight) -- satisfiesJust
import EagerWords.Common.Domain.UserDto (UserDto(UserDto))
import EagerWords.Common.Domain.InitPieces (InitPieces)
import EagerWords.Common.Domain.GameParams (GameParams)
import EagerWords.Common.Message.StartGameResponse (StartGameResponse)
import EagerWords.Server.Domain.GameEnv (GameEnv)
import EagerWords.Server.Web.GameEndPoint (saveUserHandler, startGameHandler)
import EagerWords.Server.Service.BaseServiceFixtures
import EagerWords.Common.Message.StartGameRequest (StartGameRequest(StartGameRequest))
import qualified Bolour.Util.MiscUtil as Util

mkUserWeb :: GameEnv -> String -> IO String
mkUserWeb env name = do
    userId <- Util.mkUuid
    let userDto = UserDto userId name (name ++ "@example.com")
    eitherUnit <- runHandler $ saveUserHandler env userDto
    satisfiesRight eitherUnit
    return userId

mkGameWeb :: GameEnv -> GameParams -> InitPieces -> String -> [[Int]] -> IO StartGameResponse
mkGameWeb env gameParams initPieces userId pointValues =
  satisfiesRight
    =<< runHandler (startGameHandler env (StartGameRequest gameParams initPieces userId))

