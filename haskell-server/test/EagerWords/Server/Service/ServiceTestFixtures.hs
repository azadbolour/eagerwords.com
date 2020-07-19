--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module EagerWords.Server.Service.ServiceTestFixtures (
    module EagerWords.Server.Service.BaseServiceFixtures
  , mkGame
  , testDimension
  , testTrayCapacity
  ) where

import Data.Either
import Control.Monad.Trans.Except (runExceptT)
import Bolour.Util.SpecUtil (satisfiesRight) -- satisfiesJust
-- import EagerWords.Common.Domain.Player (Player(Player))
import EagerWords.Common.Domain.InitPieces (InitPieces(InitPieces))
import EagerWords.Common.Domain.Piece (Piece)
import EagerWords.Common.Domain.PiecePoint (PiecePoint)
import EagerWords.Common.Domain.GameParams (GameParams)
import EagerWords.Server.Domain.Game (Game)
import EagerWords.Server.Domain.User
import EagerWords.Server.Domain.GameEnv (GameEnv)
import qualified EagerWords.Server.Service.GameTransformerStack as TransformerStack
import EagerWords.Server.Service.BaseServiceFixtures
import qualified EagerWords.Server.Service.GameService as GameService

mkGame :: GameEnv -> GameParams -> [PiecePoint] -> String -> [Piece] -> [Piece] -> IO Game
mkGame env gameParams initialPiecePoints userId userTrayStartsWith machineTrayStartsWith = do
  let initPieces = InitPieces initialPiecePoints userTrayStartsWith machineTrayStartsWith
  eitherResult <- runExceptT $ TransformerStack.runDefaultUnprotected env $ GameService.startGameService
    gameParams initPieces userId
  satisfiesRight eitherResult
  let (Right game) = eitherResult
  return game
