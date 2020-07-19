--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module EagerWords.Server.Web.ConvertersSpec where

import Test.Hspec
import Control.Monad.Trans.Except (runExceptT)
import EagerWords.Common.Domain.GameParams (GameParams, GameParams(GameParams))
import EagerWords.Common.Domain.InitPieces (InitPieces(InitPieces))
import EagerWords.Common.Domain.Piece (Piece, Piece(Piece))
-- import EagerWords.Server.Domain.Player (Player(Player), userIndex)
import Bolour.Plane.Domain.Point (Point, Point(Point))
import qualified Bolour.Plane.Domain.Point as Point
import EagerWords.Common.Domain.PiecePoint (PiecePoint, PiecePoint(PiecePoint))
import qualified EagerWords.Common.Domain.PiecePoint as PiecePoint
import EagerWords.Common.Message.StartGameResponse as StartGameResponse
import qualified EagerWords.Server.Domain.Game as Game
import EagerWords.Server.Domain.Tray (Tray, Tray(Tray))
import qualified EagerWords.Server.Domain.Tray as Tray
import EagerWords.Server.Domain.Board as Board
import EagerWords.Server.Web.Converters(gameToStartGameResponse)
import Bolour.Util.SpecUtil (satisfiesRight)
import qualified Bolour.Language.Domain.WordDictionary as Dict
import qualified EagerWords.Server.Domain.PieceProvider as PieceProvider
import qualified EagerWords.Common.Domain.PieceProviderType as PieceProviderType
import EagerWords.Common.Domain.GameSettings (GameSettings, GameSettings(GameSettings))
import qualified EagerWords.Common.Domain.GameSettings as GameSettings

dim = 9
mid = dim `div` 2
testUser = "You"
playerId = "1"

pointValues :: [[Int]]
pointValues = replicate dim $ replicate dim 1

testPieceProviderType = PieceProviderType.Cyclic
params = GameParams (GameSettings dim 50 12 Dict.defaultLanguageCode testPieceProviderType Nothing Nothing) pointValues
pieceProvider = PieceProvider.mkDefaultCyclicPieceProvider

spec :: Spec
spec =
  describe "convert game to game dto" $
    it "game to dto" $ do
      let playerName = "You"
      let gridPiece = PiecePoint (Piece 'E' "idE") (Point mid mid)
      let initPieces = InitPieces [gridPiece] [] []
          -- player = Player "1" playerName
      game <- satisfiesRight =<< runExceptT (Game.mkInitialGame params initPieces pieceProvider playerId)
      let response = gameToStartGameResponse game
      let brd = Game.board game
      length (Board.getPiecePoints brd) `shouldBe` length (StartGameResponse.boardPiecePoints response)
      let PiecePoint {piece, point} = head (StartGameResponse.boardPiecePoints response)
      let Point.Point {row} = point
      row `shouldBe` mid



