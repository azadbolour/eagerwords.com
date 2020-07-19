--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--


{-|
Client-side Haskell language binding for the game REST api.

Servant .Client's client function automagically generates the Haskell
language bindings by using the api specification.
-}
module EagerWords.Client.GameClient (
    addUser
  , startGame
  , commitPlay
  , machinePlay
  , swapPiece
  , closeGame
  )
  where

-- (Manager)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API ((:<|>)(..))
import Servant.Client (BaseUrl, client, ClientM)
-- import Servant.Common.Req (ClientM)
import EagerWords.Common.Domain.GameParams
import qualified EagerWords.Common.GameApi as GameApi
import EagerWords.Common.Domain.PlayPiece (PlayPiece)
import EagerWords.Common.Domain.Piece (Piece)
import EagerWords.Common.Domain.GameSettings (GameSettings)
import EagerWords.Common.Domain.GameBasicInfo (GameBasicInfo)
import EagerWords.Common.Domain.UserDto (UserDto)
import EagerWords.Common.Message.StartGameRequest (StartGameRequest)
import EagerWords.Common.Message.HandShakeResponse (HandShakeResponse)
import EagerWords.Common.Message.StartGameResponse (StartGameResponse)
import EagerWords.Common.Message.ResumeGameResponse (ResumeGameResponse)
import EagerWords.Common.Message.SwapPieceResponse (SwapPieceResponse)
import EagerWords.Common.Message.GetUserGamesRequest (GetUserGamesRequest)
import EagerWords.Common.Message.GetGamesResponse (GetGamesResponse)
import EagerWords.Common.Message.CommitPlayResponse
import EagerWords.Common.Message.MachinePlayResponse
import EagerWords.Common.Domain.GameSummary (GameSummary)

handShake :: ClientM HandShakeResponse
addUser :: UserDto -> ClientM ()
startGame :: StartGameRequest -> ClientM StartGameResponse
commitPlay :: String -> [PlayPiece] -> ClientM CommitPlayResponse
machinePlay :: String -> ClientM MachinePlayResponse
swapPiece :: String -> Piece -> ClientM SwapPieceResponse
closeGame :: String -> ClientM GameSummary
suspendGame :: String -> ClientM ()
resumeGame :: String -> ClientM ResumeGameResponse
cancelGame :: String -> ClientM ()
resignGame :: String -> ClientM ()
getUserGames :: String -> GetUserGamesRequest -> ClientM GetGamesResponse
getUnfinishedUserGames :: String -> ClientM GetGamesResponse
saveUserGameSettings :: String -> GameSettings -> ClientM ()
getUserGameSettings :: String -> ClientM (Maybe GameSettings)

handShake
  :<|> addUser
  :<|> startGame
  :<|> commitPlay
  :<|> machinePlay
  :<|> swapPiece
  :<|> closeGame
  :<|> suspendGame
  :<|> resumeGame
  :<|> cancelGame
  :<|> resignGame
  :<|> getUserGames
  :<|> getUnfinishedUserGames
  :<|> saveUserGameSettings
  :<|> getUserGameSettings
  = client GameApi.gameApi

-- Note. In Servant 6.1 we have:
--   type ClientM = ExceptT ServantError IO
-- In later versions the type has changed to something more complicated.
-- Check the API docs and look for helper functions to convert that monad to an IO.
