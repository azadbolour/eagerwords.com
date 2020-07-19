--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

{-|

Implementation of the rest endpoints for the game application.

The implementation exposes a Warp application to be used
by the main program and by integration tests. Warp is the underlying
web server.

The application depends on a Servant server, defined as a list
of handlers for corresponding API functions.

The list elements are combined by a special list combinator :\<|\>
in the same order as their corresponding functions defined in
the API interface.

The game application depends on certain configuration parameters.
Hence this module exposes an application factory, a function called
mkGameApp, that takes the game configuration as a parameter and
returns the Warp application.

See http://haskell-servant.readthedocs.io/en/stable/tutorial/Server.html

-}
module EagerWords.Server.Web.GameEndPoint (
      mkGameApp
    , saveUserHandler
    , startGameHandler
    , commitPlayHandler
    , machinePlayHandler
    , swapPieceHandler
    , closeGameHandler
    , ExceptServant
) where

import Data.ByteString.Lazy.Char8 as BS
-- import Data.Aeson (encode)

import qualified Control.Exception as Exc
import Control.Monad.Except (ExceptT(..), withExceptT)
import Control.DeepSeq (NFData)

import Network.Wai (Application)
import Servant ((:<|>)(..))
import Servant (Handler(..))

import qualified Servant.Server as Servant
-- import qualified Servant.Utils.StaticFiles as ServantStatic

import Bolour.Util.MiscUtil (debug)

import EagerWords.Common.GameApi (GameApi, gameApi)
import EagerWords.Common.Domain.Piece (Piece)
import EagerWords.Common.Domain.UserDto (UserDto, UserDto(UserDto))
import qualified EagerWords.Common.Domain.UserDto as PlayerDto
import EagerWords.Common.Domain.GameSummary (GameSummary)
import EagerWords.Common.Domain.InitPieces (InitPieces)
import EagerWords.Common.Domain.GameParams (GameParams(..))
import EagerWords.Common.Domain.PlayPiece (PlayPiece)
import EagerWords.Common.Message.HandShakeResponse (HandShakeResponse, HandShakeResponse(HandShakeResponse))
import EagerWords.Common.Message.CommitPlayResponse (CommitPlayResponse, tupleToCommitPlayResponse)
import EagerWords.Common.Message.MachinePlayResponse (MachinePlayResponse, tupleToMachinePlayResponse)
import EagerWords.Common.Message.SwapPieceResponse (SwapPieceResponse, tupleToSwapPieceResponse)
import EagerWords.Common.Message.StartGameRequest (StartGameRequest, StartGameRequest(StartGameRequest))
import qualified EagerWords.Common.Message.StartGameRequest as StartGameRequest
import EagerWords.Common.Message.StartGameResponse (StartGameResponse)
import EagerWords.Common.Message.GetGamesResponse (GetGamesResponse)
import EagerWords.Common.Message.GetUserGamesRequest (GetUserGamesRequest)
import EagerWords.Server.Domain.GameError (GameError(..), ExceptGame, encodeGameErrorWithMessage)
import EagerWords.Server.Domain.GameEnv (GameEnv(..))
import EagerWords.Server.Service.GameTransformerStack (GameTransformerStack)
import qualified EagerWords.Server.Service.GameTransformerStack as TransformerStack
import EagerWords.Server.Web.Converters (gameToStartGameResponse, userDtoToUser)
import qualified EagerWords.Server.Service.GameService as GameService
import EagerWords.Common.Message.ResumeGameResponse (ResumeGameResponse, ResumeGameResponse(ResumeGameResponse))
import EagerWords.Common.Message.GetGamesResponse (GetGamesResponse, GetGamesResponse(GetGamesResponse))
import EagerWords.Common.Domain.GameSettings (GameSettings, GameSettings(GameSettings))
import EagerWords.Common.Domain.PieceProviderType (PieceProviderType(Cyclic))

-- TODO. Simplify the api implementation by using the Servant 'enter' function.

mkGameApp :: GameEnv -> IO Application
mkGameApp env = return $ Servant.serve gameApi $ mkServer env

-- | The application server factory for the game api - based on the game environment.
mkServer :: GameEnv -> Servant.Server GameApi
mkServer env =
       handShakeHandler env
  :<|> saveUserHandler env
  :<|> startGameHandler env
  :<|> commitPlayHandler env
  :<|> machinePlayHandler env
  :<|> swapPieceHandler env
  :<|> closeGameHandler env
  :<|> suspendGameHandler env
  :<|> resumeGameHandler env
  :<|> cancelGameHandler env
  :<|> resignGameHandler env
  :<|> getUserGamesHandler env
  :<|> getUnfinishedUserGamesHandler env
  :<|> saveUserGameSettingsHandler env
  :<|> getUserGameSettingsHandler env

-- Note - In later versions of Servant this has changed - use ServantStatic.serveDirectoryFileServer "static".
-- Note also that the static handler has to be the last one in the list.
-- UI bundle deployed separately. So this is no longer needed.
{-
mkServer' :: GameEnv -> Servant.Server GameApi'
mkServer' env = mkServer env
               :<|> ServantStatic.serveDirectory "static"
               :<|> ServantStatic.serveDirectory "static"
-}

-- | Return type of api handlers required by Servant.
type ExceptServant result = ExceptT Servant.ServerError IO result
-- type ExceptServant result = ExceptT Servant.ServantErr IO result

-- | Convert a game application ExceptT to a Servant ExceptT as
--   required by the game Servant API.
exceptTAdapter :: ExceptGame result -> ExceptServant result
exceptTAdapter gameExceptT = withExceptT gameErrorToServantErr gameExceptT

-- | Convert game application errors to Servant errors.
--   Lower-level modules return GameError in case of an error.
--   Servant require API handlers that return ServantErr.
--   This function converts a GameError returned by service calls
--   to a ServantErr required by Servant.
gameErrorToServantErr :: GameError -> Servant.ServerError
-- Using 422 response code (unprocessable entity) for all errors. May want to distinguish later.
-- TODO. Use function GameError.gameErrorMessage so error messages can be specialized.
-- Default would be encode.
gameErrorToServantErr gameError = debug (show gameError) $ Servant.ServerError
    422 -- errHTTPCode
    "Unprocessable entity." -- errReasonPhrase
    -- (BS.pack $ show gameError) -- errBody
    (encodeGameErrorWithMessage gameError) -- errBody
    -- (encode gameError) -- errBody
    [] -- errHeaders

-- | Execute a game transformer stack, resolving its logger monad with
--   a fixed logging function, and resolving its reader monad
--   with a given environment, and returning a servant ExceptT as required
--   by Servant.
gameTransformerStackHandler :: (NFData result) => GameEnv -> GameTransformerStack result -> ExceptServant result
gameTransformerStackHandler env stack = exceptTAdapter $ TransformerStack.runDefault env stack

-- TODO. Pretty-printed logging for all requests and responses.
-- TODO. How to set logging level from the command line.

--
-- Servant handlers for api functions.
--

serverType :: String
serverType = "Haskell"

apiVersion :: String
apiVersion = "1.0"

handShakeResponse :: HandShakeResponse
handShakeResponse = HandShakeResponse serverType apiVersion

-- | API handler for initial handshake.
-- handShakeHandler :: GameEnv -> ExceptServant HandShakeResponse
handShakeHandler :: GameEnv -> Handler HandShakeResponse
handShakeHandler env =
  Handler $ gameTransformerStackHandler env $ return handShakeResponse

-- | API handler to insert of update a user.
saveUserHandler :: GameEnv -> UserDto -> Handler ()
saveUserHandler env userDto =
  Handler $ gameTransformerStackHandler env $ do -- GameTransformerStack
    let user = userDtoToUser userDto
    result <- GameService.saveUserService user
    -- logMessage (show result) -- TODO. Could not prettify it. Looks awful.
    return result

-- gameTransformerStackHandler env $ GameService.addPlayerService player

-- | API handler to create and start a new game.
startGameHandler :: GameEnv -> StartGameRequest -> Handler StartGameResponse
startGameHandler env (StartGameRequest{gameParams, initPieces, userId}) =
  Handler $ gameTransformerStackHandler env $ do -- GameTransformerStack
    response <- startGameServiceWrapper gameParams initPieces userId
    -- logMessage (show gameDto) -- TODO. Could not prettify it - tried groom and pretty-show. No good.
    return response

startGameServiceWrapper ::
     GameParams
  -> InitPieces
  -> String
  -> GameTransformerStack StartGameResponse
startGameServiceWrapper params userId initPieces = do
  game <- GameService.startGameService params userId initPieces
  return $ gameToStartGameResponse game

-- | API handler to commit a new play by the player side of the game.
commitPlayHandler :: GameEnv -> String -> [PlayPiece] -> Handler CommitPlayResponse
commitPlayHandler env gameId playPieces =
  Handler $ gameTransformerStackHandler env $
    tupleToCommitPlayResponse <$> GameService.commitPlayService gameId playPieces

-- | API handler to make a machine play.
machinePlayHandler :: GameEnv -> String -> Handler MachinePlayResponse
machinePlayHandler env gameId = Handler $ gameTransformerStackHandler env $
  tupleToMachinePlayResponse <$> GameService.machinePlayService gameId

-- | API handler to swap a piece.
swapPieceHandler :: GameEnv -> String -> Piece -> Handler SwapPieceResponse
swapPieceHandler env gameId piece = Handler $ gameTransformerStackHandler env $
  tupleToSwapPieceResponse <$> GameService.swapPieceService gameId piece

closeGameHandler :: GameEnv -> String -> Handler GameSummary
closeGameHandler env gameId = Handler $ gameTransformerStackHandler env $ GameService.closeGameService gameId

-- TODO. URGENT. Implement suspendGameHandler, etc.
suspendGameHandler :: GameEnv -> String -> Handler ()
suspendGameHandler env gameId =
  Handler $ gameTransformerStackHandler env $ return ()

stubGameSettings :: GameSettings
stubGameSettings = GameSettings 9 30 7 "en" Cyclic Nothing Nothing

stubGameParams :: GameParams
stubGameParams = GameParams stubGameSettings [[]]

stubResumeGameResponse :: ResumeGameResponse
stubResumeGameResponse = ResumeGameResponse "" stubGameParams [] [] 0 0

resumeGameHandler :: GameEnv -> String -> Handler ResumeGameResponse
resumeGameHandler env gameId =
  Handler $ gameTransformerStackHandler env $ return stubResumeGameResponse

cancelGameHandler :: GameEnv -> String -> Handler ()
cancelGameHandler env gameId =
  Handler $ gameTransformerStackHandler env $ return ()

resignGameHandler :: GameEnv -> String -> Handler ()
resignGameHandler env gameId =
  Handler $ gameTransformerStackHandler env $ return ()

getUserGamesHandler :: GameEnv -> String -> GetUserGamesRequest -> Handler GetGamesResponse
getUserGamesHandler env gameId request =
  Handler $ gameTransformerStackHandler env $ return $ GetGamesResponse []

getUnfinishedUserGamesHandler :: GameEnv -> String -> Handler GetGamesResponse
getUnfinishedUserGamesHandler env gameId =
  Handler $ gameTransformerStackHandler env $ return $ GetGamesResponse []

saveUserGameSettingsHandler :: GameEnv -> String -> GameSettings -> Handler ()
saveUserGameSettingsHandler env gameId settings =
  Handler $ gameTransformerStackHandler env $ return ()

getUserGameSettingsHandler :: GameEnv -> String -> Handler (Maybe GameSettings)
getUserGameSettingsHandler env gameId =
  Handler $ gameTransformerStackHandler env $ return (Just stubGameSettings)






-- | Convert an unknown exception that may be thrown by the Haskell
--   runtime or by lower-level libraries to a Servant error, as
--   needed by Servant API handlers.
exceptionToServantErr :: Exc.SomeException -> Servant.ServerError
exceptionToServantErr exception = Servant.ServerError
    500 -- errHTTPCode
    "Internal server error." -- errReasonPhrase
    (BS.pack $ show exception) -- errBody
    [] -- errHeaders

-- | Convert an unknown exception caught at the highest level
--  to the core of an ExceptT ServantErr IO monad, so it
--  can be embedded in an ExceptT ServantErr IO as required
--  by Servant. But how/where do you catch it. Catch is only for
--  the IO monad.
catchallHandler :: Exc.SomeException -> IO (Either Servant.ServerError result)
catchallHandler exception = do
  print exception -- TODO. Should log rather than print.
  return (Left $ exceptionToServantErr exception)
