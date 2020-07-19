--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module EagerWords.Server.Domain.Game (
    Game(..)
  , mkStartingGame
  , mkInitialGame
  , setPlayerTray
  , validatePlayAgainstGame
  , reflectPlayOnGame
  , doExchange
  , validateGameParams
  , gameAgeSeconds
  , toMiniState
  , summary
  , setBoard
  , setPlays
  , EagerWords.Server.Domain.Game.gameId
)
where

import Data.Int
import Data.Char (isAlphaNum)
import Data.List
import Data.Sequence (Seq, (><))
import qualified Data.Sequence as Seq
import Data.Time (UTCTime)
import Data.Time.Clock (diffUTCTime)
import Data.Time
import Data.Time.Clock.POSIX

import Control.Monad.IO.Class (MonadIO(..), liftIO)
import Control.Monad.Except (MonadError(..))

import qualified Bolour.Util.MiscUtil as Util
import Bolour.Util.TimeUtil (nowSecs)

import EagerWords.Common.Domain.GameParams as GameParams
import EagerWords.Common.Domain.InitPieces (InitPieces, InitPieces(InitPieces))
import qualified EagerWords.Common.Domain.InitPieces as InitPieces
import EagerWords.Common.Domain.PiecePoint (PiecePoint, PiecePoint(PiecePoint))
import qualified EagerWords.Common.Domain.PiecePoint as PiecePoint
import Bolour.Plane.Domain.Point (Point)
import qualified Bolour.Plane.Domain.Point as Point
import EagerWords.Common.Domain.Piece (Piece)
-- import EagerWords.Server.Domain.Player (Player)
import EagerWords.Server.Domain.Play (Play)
import qualified EagerWords.Server.Domain.Play as Play (mkWordPlay, mkSwapPlay)
import EagerWords.Common.Domain.PlayerType (PlayerType(..))
import qualified EagerWords.Common.Domain.PlayerType as PlayerType
import qualified EagerWords.Common.Domain.PlayerType as Player
import EagerWords.Common.Domain.PlayPiece (PlayPiece)
import qualified EagerWords.Common.Domain.PlayPiece as PlayPiece
import EagerWords.Common.Domain.GameMiniState (GameMiniState, GameMiniState(GameMiniState))
import EagerWords.Common.Domain.GameSummary (GameSummary, GameSummary(GameSummary))
import EagerWords.Common.Domain.StopInfo (StopInfo, StopInfo(StopInfo))

import EagerWords.Server.Domain.Board (Board)
import qualified EagerWords.Server.Domain.Board as Board
import EagerWords.Server.Domain.Tray (Tray, Tray(Tray))
import qualified EagerWords.Server.Domain.Tray as Tray
import EagerWords.Server.Domain.GameError
import EagerWords.Server.Domain.PieceProvider (PieceProvider)
import qualified EagerWords.Server.Domain.PieceProvider as PieceProvider
import qualified EagerWords.Server.Domain.Scorer as Scorer
import EagerWords.Server.Domain.GameBase (GameBase, GameBase(GameBase))
import qualified EagerWords.Server.Domain.GameBase as GameBase
import EagerWords.Common.Domain.GameSettings (GameSettings, GameSettings(GameSettings))
import qualified EagerWords.Common.Domain.GameSettings as GameSettings

data Game = Game {
    gameBase :: GameBase
  , board :: Board
  , trays :: [Tray.Tray] -- ^ Indexed by Player.playerTypeIndex UserPlayer or MachinePlayer. User 0, Machine 1.
  , playNumber :: Int
  , playTurn :: PlayerType
  , pieceProvider :: PieceProvider
  , scorePlay :: [PlayPiece] -> Int
  , lastPlayScore :: Int
  , scores :: [Int]
  , numSuccessivePasses :: Int
  , plays :: Seq Play
  , lastSecond :: Int64
}

noScore = 0
noScores = [0, 0]
initPlayNumber = 0

maxDimension = 120
maxTraySize = 26
maxSuccessivePasses = 10
noPasses = 0
noPlays :: Seq Play
noPlays = Seq.empty

gameId :: Game -> String
gameId Game {gameBase} = GameBase.gameId gameBase

mkStartingGame :: GameBase -> PieceProvider -> IO Game
mkStartingGame base pieceProvider = do
  let GameBase {gameParams, initPieces} = base
      GameParams {settings = GameSettings {dimension, trayCapacity}, pointValues} = gameParams
      InitPieces {userPieces, machinePieces, boardPiecePoints} = initPieces
      board = Board.mkBoardFromPiecePoints boardPiecePoints dimension
      scorer = Scorer.scorePlay $ Scorer.mkScorer pointValues
      initPlayerType = Player.User -- Won't be used. Arbitrarily set to user.
  lastSeconds <- nowSecs
  (userTray, pieceProvider') <- Tray.mkTray pieceProvider trayCapacity userPieces
  (machineTray, pieceProvider'') <- Tray.mkTray pieceProvider' trayCapacity machinePieces
  let initTrays = [userTray, machineTray]
      game = Game base board initTrays initPlayNumber initPlayerType pieceProvider''
          scorer noScore noScores noPasses noPlays lastSeconds
  return game

-- TODO. Complete show of game if needed.
instance Show Game where
  show game @ Game {board} = "Game {" ++ "board" ++ show board ++ "}"

passesMaxedOut :: Game -> Bool
passesMaxedOut Game { numSuccessivePasses } = numSuccessivePasses == maxSuccessivePasses

setBoard :: Game -> Board -> Game
setBoard game b = game {board = b}

setPlays :: Game -> Seq Play -> Game
setPlays game plays = game {plays = plays}

stopInfo :: Game -> StopInfo
stopInfo game @ Game { board, numSuccessivePasses } =
  StopInfo numSuccessivePasses (Board.isFilled board)

updatePieceGenerator :: Game -> PieceProvider -> Game

-- | Explicit record update for fieldGenerator.
--   Cannot do normal update: game {pieceProvider = nextGen} gets compiler error:
--   Record update for insufficiently polymorphic field: pieceProvider.
updatePieceGenerator game generator = game { pieceProvider = generator }

-- | Note. Validation of player name does not happen here.
mkInitialGame :: (MonadError GameError m, MonadIO m) =>
  GameParams -> InitPieces -> PieceProvider -> String -> m Game
mkInitialGame gameParams initPieces pieceProvider playerId = do
  base <- liftIO $ GameBase.mkInitialBase gameParams initPieces playerId
  liftIO $ mkStartingGame base pieceProvider

toMiniState :: Game -> GameMiniState
toMiniState game @ Game {board, pieceProvider, lastPlayScore, scores} =
  let
      gameEnded = passesMaxedOut game || Board.isFilled board
  in GameMiniState lastPlayScore scores gameEnded

gameAgeSeconds :: UTCTime -> Game -> Int64
gameAgeSeconds utcNow game @ Game {gameBase} =
  let GameBase {firstSecond} = gameBase
      nowSeconds = floor $ utcTimeToPOSIXSeconds utcNow
      ageSeconds = nowSeconds - firstSecond
  in ageSeconds

summary :: Game -> GameSummary
summary game @ Game {trays, scores} =
  let stopData = stopInfo game
  in GameSummary stopData

setPlayerTray :: Game -> PlayerType -> Tray -> Game
setPlayerTray (game @ Game {trays}) playerType tray =
  let whichTray = Player.playerTypeIndex playerType
      trays' = Util.setListElement trays whichTray tray
  in game {trays = trays'}

-- TODO. The following are only needed for player play.
-- Machine play already satisfies them.
-- TODO. The following are basic sanity checks. Not exhaustive yet. And some may be stubbed. Fix.
-- | Make sure the incoming play is consistent with the state of the game.
validatePlayAgainstGame :: MonadError GameError m => Game -> [PlayPiece] -> m [PlayPiece]
validatePlayAgainstGame game playPieces =
      checkContiguousPlay playPieces
  >>= checkPlayLineInBounds game
  >>= checkMoveDestinationsEmpty game
  >>= checkMoveTrayPieces game
  >>= checkPlayPositionsOccupied game
  >>= checkPlayBoardPieces game

-- TODO. Check connectivity. Either has to have an anchor or be a parallel play.
-- That is check in teh UI for now. So OK to defer.
checkContiguousPlay :: MonadError GameError m => [PlayPiece] -> m [PlayPiece]
checkContiguousPlay playPieces =
  let points = PlayPiece.point <$> playPieces
      contiguousBy f = Util.contiguous (f <$> points)
  in if contiguousBy Point.row || contiguousBy Point.col then
       return playPieces
     else throwError $ NonContiguousPlayError points

checkPlayLineInBounds :: MonadError GameError m => Game -> [PlayPiece] -> m [PlayPiece]
checkPlayLineInBounds _ [] = return []
checkPlayLineInBounds (Game { board }) playPieces = do
  Board.validatePoint board (PlayPiece.point (head playPieces))
  Board.validatePoint board (PlayPiece.point (last playPieces))
  return playPieces

-- Assume grid points have been checked to be in bounds.
checkMoveDestinationsEmpty :: MonadError GameError m => Game -> [PlayPiece] -> m [PlayPiece]
checkMoveDestinationsEmpty Game {board} playPieces =
  let gridPoints = PlayPiece.point <$> filter PlayPiece.moved playPieces
      maybeTaken = find (Board.pointIsNonEmpty board) gridPoints
  in case maybeTaken of
     Nothing -> return playPieces
     Just taken -> throwError $ OccupiedMoveDestinationError taken

-- TODO. Check cross words. Rename.
-- checkMoveDestinationsFreeCrossWise :: MonadError GameError m => Game -> [PlayPiece] -> m [PlayPiece]
-- checkMoveDestinationsFreeCrossWise game = return

-- TODO. Implement validation.
checkMoveTrayPieces :: MonadError GameError m => Game -> [PlayPiece] -> m [PlayPiece]
checkMoveTrayPieces game = return

-- | Check that the existing board pieces in the purported play are
--   in fact occupied already.
checkPlayPositionsOccupied :: MonadError GameError m => Game -> [PlayPiece] -> m [PlayPiece]
checkPlayPositionsOccupied Game {board} playPieces =
  let boardPlayPieces = filter (not . PlayPiece.moved) playPieces
      maybeFreePlayPiece = find (not . Board.pointHasValue board . PlayPiece.point) boardPlayPieces
  in case maybeFreePlayPiece of
     Nothing -> return playPieces
     Just missingPlayPiece -> throwError $ MissingBoardPlayPieceError $ PlayPiece.getPiecePoint missingPlayPiece

-- | Check that purported play pieces already on the board indicated on a play
--   do exist on the board.
checkPlayBoardPieces :: MonadError GameError m => Game -> [PlayPiece] -> m [PlayPiece]
checkPlayBoardPieces Game {board} playPieces =
  let boardPlayPieces = filter (not . PlayPiece.moved) playPieces
      piecePoints = PlayPiece.getPiecePoint <$> boardPlayPieces
      piecePointMatchesBoard PiecePoint {piece, point} =
        let maybePiece = Board.getPiece board point
        in case maybePiece of
           Nothing -> False
           Just boardPiece -> boardPiece == piece
      maybeMismatch = find (not . piecePointMatchesBoard) piecePoints
  in case maybeMismatch of
       Nothing -> return playPieces
       Just piecePoint -> throwError $ UnmatchedBoardPlayPieceError piecePoint

reflectPlayOnGame :: (MonadError GameError m, MonadIO m) =>
  Game -> PlayerType -> [PlayPiece] -> (Board -> (Board, [Point])) -> m (Game, [Piece], [Point])

reflectPlayOnGame game playerType playPieces deadPointFinder = do
  let Game {board, trays, playNumber, pieceProvider, numSuccessivePasses, scorePlay, scores, plays} = game
  _ <- if playerType == PlayerType.User then validatePlayAgainstGame game playPieces else return playPieces
  let movedPlayPieces = filter PlayPiece.moved playPieces
      movedPiecePoints = PlayPiece.getPiecePoint <$> movedPlayPieces
      b = Board.setPiecePoints board movedPiecePoints
      (b', deadPoints) = deadPointFinder b
      usedPieces = PiecePoint.piece <$> movedPiecePoints
      playerIndex = Player.playerTypeIndex playerType
  (newPieces, pieceProvider') <- liftIO $ PieceProvider.takePieces pieceProvider (length usedPieces)
  lastSecond <- liftIO nowSecs
  let game' = game {pieceProvider = pieceProvider'}
  let tray = trays !! playerIndex
      tray' = Tray.replacePieces tray usedPieces newPieces
      trays' = Util.setListElement trays playerIndex tray'
      playNumber' = playNumber + 1
      earlierScore = scores !! playerIndex
      thisScore = scorePlay playPieces
  let score' = earlierScore + thisScore
      scores' = Util.setListElement scores playerIndex score'
      play =  Play.mkWordPlay playNumber' playerType scores' playPieces newPieces deadPoints
      plays' = plays >< Seq.singleton play
      game'' = game' { board = b', trays = trays', playNumber = playNumber', lastPlayScore = thisScore, numSuccessivePasses = 0, 
      scores = scores', plays = plays', lastSecond = lastSecond}
  return (game'', newPieces, deadPoints)

doExchange :: (MonadError GameError m, MonadIO m) => Game -> PlayerType -> Int -> m (Game, Piece)
doExchange (game @ Game {board, trays, pieceProvider, playNumber, numSuccessivePasses, scores, plays}) playerType trayPos = do
  let whichTray = Player.playerTypeIndex playerType
      tray @ Tray {pieces} = trays !! whichTray
      piece = pieces !! trayPos -- TODO. Defensive check.
      succPasses = numSuccessivePasses + 1
      playNumber' = playNumber + 1
      game' = game { numSuccessivePasses = succPasses, lastPlayScore = 0 , playNumber = playNumber'}
  (piece', pieceProvider1) <- liftIO $ PieceProvider.swapOne pieceProvider piece
  lastSecond <- liftIO nowSecs
  -- TODO. Reduce constructions.
  let play =  Play.mkSwapPlay playNumber' playerType scores piece piece'
      plays' = plays >< Seq.singleton play
      game'' = game' { pieceProvider = pieceProvider1, plays = plays', lastSecond = lastSecond }
      tray' = Tray.replacePiece tray trayPos piece'
      game''' = setPlayerTray game'' playerType tray'
  return (game''', piece')

-- Either is a MonadError - but this generalizes the function.
-- In general rather than using a specific monad, use an mtl type class
-- that abstracts the specific functions provided by that monad.
-- Here rather than Either we use MonadError.
validateGameParams :: MonadError GameError m => GameParams -> m GameParams
validateGameParams params =
  let GameParams.GameParams {settings = GameSettings {dimension, trayCapacity}} = params
  in
    if dimension <= 0 || dimension > maxDimension then
      throwError $ InvalidDimensionError dimension
    else if trayCapacity <= 0 || trayCapacity > maxTraySize then
      throwError $ InvalidTrayCapacityError trayCapacity
    else return params
