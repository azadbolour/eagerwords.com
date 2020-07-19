--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

{-|
The service layer of the game application. This layer is independent of
communication concerns with clients.
-}
module EagerWords.Server.Service.GameService (
    saveUserService
  , startGameService
  , commitPlayService
  , machinePlayService
  , swapPieceService
  , closeGameService
  , timeoutLongRunningGames
  , prepareDb
  , unknownPlayerName
  , mkUnknownUser
  )
  where

import Data.Int
-- import Data.List
import Data.Maybe (fromJust, isNothing)
import Data.Time (getCurrentTime)
import Data.Bool (bool)
import qualified Data.Map as Map

import Control.Monad (when, unless, sequence)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Except (MonadError(..), withExceptT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Reader (MonadReader(..), asks, ask)
import Control.Monad.Trans.Class (lift)

import Bolour.Util.MiscUtil (isAlphaNumString)
-- import Bolour.Util.Core (EntityId)
import qualified Bolour.Util.MiscUtil as Util
import Bolour.Util.FrequencyDistribution (FrequencyDistribution(..))
import qualified Bolour.Util.FrequencyDistribution as FrequencyDistribution

import Bolour.Plane.Domain.Point (Point, Point(Point))
import qualified Bolour.Plane.Domain.Axis as Axis
import EagerWords.Server.Domain.User (User, User(User))
import qualified EagerWords.Server.Domain.User as User

import Bolour.Language.Util.WordUtil (DictWord)
import qualified Bolour.Language.Domain.WordDictionary as Dict
import Bolour.Language.Domain.WordDictionary (WordDictionary)
import qualified Bolour.Language.Domain.DictionaryCache as DictionaryCache

import EagerWords.Common.Domain.PieceProviderType (PieceProviderType(..))
import qualified EagerWords.Common.Domain.PieceProviderType as PieceProviderType
-- import EagerWords.Common.Domain.PlayerType (PlayerType(..))
import qualified EagerWords.Common.Domain.PlayerType as PlayerType
import EagerWords.Common.Domain.Piece (Piece, Piece(Piece))
import qualified EagerWords.Common.Domain.Piece as Piece
import EagerWords.Common.Domain.GameMiniState (GameMiniState)
import EagerWords.Common.Domain.GameSummary (GameSummary)
-- import EagerWords.Common.Domain.PiecePoint (PiecePoint)
import EagerWords.Common.Domain.PlayPiece (PlayPiece, PlayPiece(PlayPiece))
import qualified EagerWords.Common.Domain.PlayPiece as PlayPiece
import EagerWords.Common.Domain.GameParams (GameParams, GameParams(..))

import qualified EagerWords.Server.Domain.ServerVersion as ServerVersion
import EagerWords.Server.Domain.Game (Game, Game(Game))
import qualified EagerWords.Server.Domain.Game as Game
-- import EagerWords.Server.Domain.GameBase (GameBase, GameBase(GameBase))
import qualified EagerWords.Server.Domain.GameBase as GameBase
import EagerWords.Common.Domain.InitPieces (InitPieces, InitPieces(InitPieces))
import qualified EagerWords.Common.Domain.InitPieces as InitPieces

import EagerWords.Server.Domain.GameBase (GameBase(GameBase))
-- import qualified EagerWords.Server.Domain.GameBase as GameBase
import EagerWords.Server.Domain.GameError (GameError(..))
import EagerWords.Server.Domain.Tray (Tray(Tray))
import qualified EagerWords.Server.Domain.Tray as Tray
import qualified EagerWords.Server.Domain.Board as Board
import EagerWords.Server.Domain.Board (Board)
import qualified EagerWords.Server.Domain.GameCache as GameCache
import qualified EagerWords.Server.Domain.CrossWordFinder as CrossWordFinder
import EagerWords.Server.Domain.GameEnv (GameEnv(..))
import EagerWords.Server.Service.GameTransformerStack (GameTransformerStack, exceptTToStack)
import qualified EagerWords.Server.Domain.GameEnv as GameEnv (GameEnv(..))
import EagerWords.Server.Domain.ServerConfig as ServerConfig
import qualified EagerWords.Server.Domain.StripMatcher as Matcher
import qualified EagerWords.Server.Domain.Strip as Strip
import EagerWords.Server.Domain.Strip (Strip, Strip(Strip))
import EagerWords.Server.Domain.PieceProvider (PieceProvider(..))
import qualified EagerWords.Server.Service.GameLetterDistribution as GameLetterDistribution
import EagerWords.Server.Service.GameData (GameData, GameData(GameData))
import qualified EagerWords.Server.Service.GameData as GameData

import EagerWords.Server.Service.GamePersister (GamePersister, GamePersister(GamePersister))
import qualified EagerWords.Server.Service.GamePersister as GamePersister
import qualified EagerWords.Server.Service.GamePersisterJsonBridge as GamePersisterJsonBridge
import qualified EagerWords.Server.Service.GameJsonSqlPersister as GameJsonSqlPersister
-- import qualified EagerWords.Server.Service.GameJsonSqlPersister as GamePersister
import EagerWords.Common.Domain.GameSettings (GameSettings, GameSettings(GameSettings))
import qualified EagerWords.Common.Domain.GameSettings as GameSettings

mkPersister :: GameTransformerStack GamePersister
mkPersister = do
  connectionProvider <- asks GameEnv.connectionProvider
  let jsonPersister = GameJsonSqlPersister.mkPersister connectionProvider
      version = ServerVersion.version
  return $ GamePersisterJsonBridge.mkBridge jsonPersister version

-- TODO. URGENT. Remove unknown user.
unknownPlayerName = "You"

-- TODO. URGENT. Temporary duplicated code from GameService. Remove.
mkUnknownUser :: IO User
mkUnknownUser = do
  userId <- Util.mkUuid
  let name = unknownPlayerName
  return $ User userId name (name ++ "@example.com")



-- unknownPlayer = Player unknownPlayerName

-- | Convert game data stored in the database to a game domain object.
--   Important. The game's piece provider depends on specific parameters
--   that are not stored in the database (for simplicity).
--   It is assumed that these parameters are immutable constants over
--   versions of the system. They are defined in this module.
--   Furthermore, the state of a piece provider changes during the
--   the game, and that state is also not tracked in the database.
--   So the state of the piece provider in a restored game may be
--   different than that of the saved game. For the use cases of
--   our current piece providers this is not a major issue.
--   Random piece providers should function as expected. And
--   cyclic piece providers are only used in testing.
gameFromData :: GameData -> IO Game
gameFromData GameData {base, plays} = do
  let GameBase {gameParams} = base
      GameParams {settings = GameSettings {pieceProviderType}} = gameParams
      pieceProvider = mkPieceProvider pieceProviderType
  game <- Game.mkStartingGame base pieceProvider
  let game' = Game.setPlays game plays
      -- TODO. Update the state of the game using the plays.
  return game'

-- | Convert a game domain object to its db-storable representation.
--   Note that the parameters of the piece provider of the game
--   are not stored in the database. For now the potential change
--   of these parameters over time is a non-requirement.
dataFromGame :: Game -> GameData
dataFromGame Game {gameBase, plays} = GameData gameBase plays

prepareDb :: GameTransformerStack ()
prepareDb = do
  GamePersister {migrate} <- mkPersister
  exceptTToStack migrate

timeoutLongRunningGames :: GameTransformerStack ()
timeoutLongRunningGames = do
  gameCache <- asks GameEnv.gameCache
  serverConfig <- asks GameEnv.serverConfig
  let ServerConfig {maxGameMinutes} = serverConfig
  gamesMap <- GameCache.cacheGetGamesMap gameCache
  utcNow <- liftIO getCurrentTime
  let games = Map.elems gamesMap
      limit :: Int64
      limit = fromIntegral maxGameMinutes * 60
      agedGameIds = let aged = (limit <) . Game.gameAgeSeconds utcNow
                    in Game.gameId <$> aged `filter` games
  exceptTToStack $ GameCache.deleteItems agedGameIds gameCache
  -- TODO. End the games in the database with a timed out indication.

existsOk = True
existsNotOk = not existsOk

-- | Service function to add a player to the system - error if name exists.
saveUserService :: User -> GameTransformerStack ()
saveUserService user = do
  persister @ GamePersister {saveUser} <- mkPersister
  exceptTToStack $ saveUser user

-- TODO. Move to GameError.
type GameIOEither a = IO (Either GameError a)

stringExceptLifter :: ExceptT String IO a -> GameTransformerStack a
stringExceptLifter except =
  let errorMapper = InternalError
      exceptGame = errorMapper `withExceptT` except
  in lift (lift exceptGame)

lookupDictionary :: String -> GameTransformerStack WordDictionary
lookupDictionary languageCode = do
  GameEnv {dictionaryCache} <- ask
  stringExceptLifter $ DictionaryCache.lookup languageCode dictionaryCache

-- | Service function to create and start a new game.
startGameService ::
     GameParams   -- Basic game parameters - dimension, etc.
  -> InitPieces   -- Initial pieces in trays and on board - for testing.
  -> String
  -> GameTransformerStack Game

startGameService gameParams initPieces userId = do
  -- TODO. Validate parameters. Point values must be a dimension x dimension matrix.
  params <- Game.validateGameParams gameParams
  let GameParams {settings = GameSettings{dimension, languageCode, pieceProviderType}} = params
  let InitPieces {userPieces, machinePieces} = initPieces
  GameEnv { gameCache } <- ask
  persister @ GamePersister { findUserByUserId} <- mkPersister
  maybeUser <- exceptTToStack $ findUserByUserId userId
  case maybeUser of
    Nothing -> throwError $ MissingPlayerError userId
    Just user -> do
      let pieceProvider = mkPieceProvider pieceProviderType
          User {userId} = user
      game <- Game.mkInitialGame params initPieces pieceProvider userId
      persistGame game
      exceptTToStack $ GameCache.insert game gameCache
      return game

restoreGameService :: String -> GameTransformerStack (Maybe Game)
restoreGameService gameId = do
  persister @ GamePersister {findGameById} <- mkPersister
  maybeGameData <- exceptTToStack $ findGameById gameId
  liftIO $ sequence $ gameFromData <$> maybeGameData

persistGame :: Game -> GameTransformerStack ()
persistGame game = do
  persister <- mkPersister
  exceptTToStack $ GamePersister.saveGame persister $ dataFromGame game

validateCrossWords :: Board -> WordDictionary -> Strip -> String -> GameTransformerStack ()
validateCrossWords board dictionary strip word = do
  let crosswords = CrossWordFinder.findStripCrossWords board strip word
      invalidCrosswords = filter (not . Dict.isWord dictionary) crosswords
  bool (throwError $ InvalidCrossWordError invalidCrosswords) (return ()) (null invalidCrosswords)

-- | Service function to commit a user play - reflecting it on the
--   game's board, and and refilling the user tray.
--   Return the newly added replenishment pieces to the user tray.
commitPlayService ::
     String
  -> [PlayPiece]
  -> GameTransformerStack (GameMiniState, [Piece], [Point])

commitPlayService gameId playPieces = do
  GameEnv { gameCache } <- ask
  game @ Game {gameBase, board} <- exceptTToStack $ GameCache.lookup gameId gameCache
  let GameBase {gameParams} = gameBase
      GameParams {settings = GameSettings {languageCode}} = gameParams
      playWord = PlayPiece.playPiecesToWord playPieces
  dictionary <- lookupDictionary languageCode
  unless (Dict.isWord dictionary playWord) $
    throwError $ InvalidWordError playWord
  let maybeStrip = Board.stripOfPlay board playPieces
  when (isNothing maybeStrip) $
    throwError $ WordTooShortError playWord
  let Just strip = maybeStrip
  validateCrossWords board dictionary strip playWord
  let blackPointFinder = Matcher.findAndSetBoardBlackPoints dictionary
  (game', refills, deadPoints)
    <- Game.reflectPlayOnGame game PlayerType.User playPieces blackPointFinder
  persistGame game'
  exceptTToStack $ GameCache.insert game' gameCache
  let miniState = Game.toMiniState game'
  return (miniState, refills, deadPoints)

-- | Service function to obtain the next machine play.
--   If no match is found, then the machine exchanges a piece.
machinePlayService :: String -> GameTransformerStack (GameMiniState, [PlayPiece], [Point])
machinePlayService gameId = do
  GameEnv { gameCache } <- ask
  (game @ Game {gameBase, board, trays}) <- exceptTToStack $ GameCache.lookup gameId gameCache
  let gameId = Game.gameId game
      GameBase {gameParams} = gameBase
      GameParams {settings = GameSettings{languageCode}} = gameParams
  dictionary <- lookupDictionary languageCode
  let machineTray @ Tray {pieces} = trays !! PlayerType.machineIndex
      trayChars = Piece.value <$> pieces
      maybeMatch = Matcher.findOptimalMatch dictionary board trayChars
  (game', machinePlayPieces, deadPoints) <- case maybeMatch of
    Nothing -> do
      gm <- exchangeMachinePiece game
      return (gm, [], []) -- If no pieces were used - we know it was a swap.
    Just (strip, word) -> do
      (playPieces, depletedTray) <- stripMatchAsPlay board machineTray strip word
      let blackPointFinder = Matcher.findAndSetBoardBlackPoints dictionary
      (gm, refills, deadPoints)
        <- Game.reflectPlayOnGame game PlayerType.Machine playPieces blackPointFinder
      return (gm, playPieces, deadPoints)
  persistGame game'
  exceptTToStack $ GameCache.insert game' gameCache
  let miniState = Game.toMiniState game'
  return (miniState, machinePlayPieces, deadPoints)

-- | Service function to swap a user piece for another.
swapPieceService :: String -> Piece -> GameTransformerStack (GameMiniState, Piece)

swapPieceService gameId (piece @ Piece {id}) = do
  gameCache <- asks GameEnv.gameCache
  (game @ Game {trays}) <- exceptTToStack $ GameCache.lookup gameId gameCache
  let gameId = Game.gameId game
      (userTray @ Tray {pieces}) = trays !! PlayerType.userIndex
  index <- Tray.findPieceIndexById userTray id
  let swappedPiece = pieces !! index
  (game', newPiece) <- Game.doExchange game PlayerType.User index
  exceptTToStack $ GameCache.insert game' gameCache
  persistGame game'
  let miniState = Game.toMiniState game'
  return (miniState, newPiece)

-- | No matches available for machine - do a swap instead.
exchangeMachinePiece :: Game -> GameTransformerStack Game
exchangeMachinePiece (game @ Game {trays}) = do
  let gameId = Game.gameId game
      (machineTray @ Tray {pieces}) = trays !! PlayerType.machineIndex
  if Tray.isEmpty machineTray
    then return game
    else do
      let piece @ Piece { id } = head pieces
      index <- Tray.findPieceIndexById machineTray id
      (game', newPiece) <- Game.doExchange game PlayerType.Machine index
      return game'

closeGameService :: String -> GameTransformerStack GameSummary
closeGameService gameId = do
  gameCache <- asks GameEnv.gameCache
  game <- exceptTToStack $ GameCache.lookup gameId gameCache
  exceptTToStack $ GameCache.delete gameId gameCache
  return $ Game.summary game
  -- TODO. Tell the database that the game has ended - as opposed to suspended.
  -- TODO. Game.summary should return the game updated with the bonus/penalty scores.
  -- TODO. Persist that final state of the game.

stripPoint :: Strip -> Int -> Point
stripPoint (strip @ Strip {axis, lineNumber, begin}) offset =
  case axis of
    Axis.X -> Point lineNumber (begin + offset)
    Axis.Y -> Point (begin + offset) lineNumber

-- | Effect of a strip match in terms of play pieces.
stripMatchAsPlay :: (MonadError GameError m, MonadIO m) => Board -> Tray -> Strip -> DictWord -> m ([PlayPiece], Tray)

stripMatchAsPlay board tray strip word = do
  let playPiecePeeler [] position (playPieces, tray) = return (playPieces, tray)
      playPiecePeeler (wordHead : wordTail) position (playPieces, tray) = do
        let point = stripPoint strip position
            maybePiece = Board.getPiece board point
            moved = isNothing maybePiece
        (piece', tray') <- if not moved then return (fromJust maybePiece, tray)
                           else Tray.removePieceByValue tray wordHead
        let playPiece = PlayPiece piece' point moved
        playPiecePeeler wordTail (position + 1) (playPiece : playPieces, tray')
  (reversePlayPieces, depletedTray) <- playPiecePeeler word 0 ([], tray)
  return (reverse reversePlayPieces, depletedTray)

letterDistribution :: FrequencyDistribution Char
letterDistribution = GameLetterDistribution.letterDistribution

caps = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

mkPieceProvider :: PieceProviderType -> PieceProvider
mkPieceProvider PieceProviderType.Random =
  let randomizer = FrequencyDistribution.randomValue letterDistribution
  in RandomPieceProvider 0 randomizer
mkPieceProvider PieceProviderType.Cyclic = CyclicPieceProvider 0 (cycle caps)
