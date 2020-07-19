--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module EagerWords.Server.Web.GameEndPointSpec (
    spec
  ) where

import Test.Hspec
import Data.Char (isUpper)
import Control.Monad.Trans.Except (runExceptT)
import Servant(runHandler)

import Bolour.Util.SpecUtil (satisfiesRight)
import EagerWords.Common.Message.SwapPieceResponse (SwapPieceResponse(..))
import EagerWords.Common.Message.StartGameResponse (StartGameResponse(StartGameResponse))
import qualified EagerWords.Common.Message.StartGameResponse as StartGameResponse(StartGameResponse(..))
import EagerWords.Common.Domain.Piece (Piece(Piece))
import qualified EagerWords.Common.Domain.Piece as Piece
import EagerWords.Common.Domain.InitPieces (InitPieces(InitPieces))
import Bolour.Plane.Domain.Point (Point(Point))
import EagerWords.Common.Domain.PlayPiece (PlayPiece(PlayPiece))
import qualified EagerWords.Common.Domain.PlayPiece as PlayPiece

import EagerWords.Common.Message.CommitPlayResponse (CommitPlayResponse(CommitPlayResponse))
import qualified EagerWords.Common.Message.CommitPlayResponse as CommitPlayResponse
import EagerWords.Common.Message.MachinePlayResponse (MachinePlayResponse(MachinePlayResponse))
import qualified EagerWords.Common.Message.MachinePlayResponse as MachinePlayResponse

import EagerWords.Server.Web.GameEndPoint (
    commitPlayHandler
  , machinePlayHandler
  , swapPieceHandler
  )
import qualified EagerWords.Server.Web.WebTestFixtures as Fixtures (
      mkUserWeb
    , mkGameWeb
    , testUser
    , testGameParams
    , initTest
    , testDimension
    , testTrayCapacity
  )

-- TODO. Annotate spec do statements with the demystified type of their monad.
-- TODO. Factor out common test functions to a base type class.

-- TODO. Test with games of dimension 1 as a boundary case.

dim = Fixtures.testDimension

pointValues :: [[Int]]
pointValues = replicate dim $ replicate dim 1

spec :: Spec
spec = do
  describe "start a game" $
    it "starts game" $
      do
        env <- Fixtures.initTest
        userId <- Fixtures.mkUserWeb env "You"
        -- let gameParams = Fixtures.mkGameParams userId
        gameDto <- Fixtures.mkGameWeb env Fixtures.testGameParams (InitPieces [] [] []) userId pointValues
        length (StartGameResponse.trayPieces gameDto) `shouldSatisfy` (== Fixtures.testTrayCapacity)

  describe "commits a play" $
    it "commit a play" $
      do
        env <- Fixtures.initTest
        userId <- Fixtures.mkUserWeb env "You"
        let gameParams = Fixtures.testGameParams
        let uPieces = [Piece 'B' "1", Piece 'E' "2", Piece 'T' "3"] -- Allow the word 'BET'
            mPieces = [Piece 'S' "4", Piece 'T' "5", Piece 'Z' "6"] -- Allow the word 'SET' across.
        StartGameResponse {gameId, trayPieces}
          <- Fixtures.mkGameWeb env gameParams (InitPieces [] uPieces mPieces) userId pointValues
        let pc0:pc1:pc2:_ = uPieces
            center = Fixtures.testDimension `div` 2
            playPieces = [
                PlayPiece pc0 (Point center (center - 1)) True
              , PlayPiece pc1 (Point center center) True
              , PlayPiece pc2 (Point center (center + 1)) True
              ]

        CommitPlayResponse {gameMiniState, replacementPieces} <- satisfiesRight
          =<< runHandler (commitPlayHandler env gameId playPieces)
        length replacementPieces `shouldBe` 3

  describe "make machine play" $
    it "make machine play" $
      do
        env <- Fixtures.initTest
        userId <- Fixtures.mkUserWeb env "You"
        let gameParams = Fixtures.testGameParams
        gameDto <- Fixtures.mkGameWeb env gameParams (InitPieces [] [] []) userId pointValues
        MachinePlayResponse {gameMiniState, playedPieces} <- satisfiesRight
          =<< runHandler (machinePlayHandler env (StartGameResponse.gameId gameDto))
        let word = PlayPiece.playPiecesToWord playedPieces
        length word `shouldSatisfy` (> 1)
  describe "swap a piece" $
    it "swap a piece" $
      do
        env <- Fixtures.initTest
        userId <- Fixtures.mkUserWeb env "You"
        let gameParams = Fixtures.testGameParams
        (StartGameResponse {gameId, trayPieces}) <- Fixtures.mkGameWeb env gameParams (InitPieces [] [] []) userId pointValues
        let piece = head trayPieces
        SwapPieceResponse {gameMiniState, piece = swappedPiece} <- satisfiesRight
          =<< runHandler (swapPieceHandler env gameId piece)
        let Piece {value} = swappedPiece
        value `shouldSatisfy` isUpper


