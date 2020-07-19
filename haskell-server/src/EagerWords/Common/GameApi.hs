--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{-|
Servant API for board game.

See http://haskell-servant.readthedocs.io/en/stable/tutorial/Server.html

-}

module EagerWords.Common.GameApi (
    GameApi
  , gameApi
)
where

import Servant
-- import EagerWords.Common.Domain.GameParams (GameParams)
import EagerWords.Common.Domain.PlayPiece (PlayPiece)
import EagerWords.Common.Message.HandShakeResponse (HandShakeResponse)
import EagerWords.Common.Message.CommitPlayResponse (CommitPlayResponse)
import EagerWords.Common.Message.MachinePlayResponse (MachinePlayResponse)
import EagerWords.Common.Message.StartGameRequest (StartGameRequest)
import EagerWords.Common.Message.StartGameResponse (StartGameResponse)
import EagerWords.Common.Message.ResumeGameResponse (ResumeGameResponse)
import EagerWords.Common.Message.SwapPieceResponse (SwapPieceResponse)
import EagerWords.Common.Message.GetGamesResponse (GetGamesResponse)
import EagerWords.Common.Message.GetUserGamesRequest (GetUserGamesRequest)
import EagerWords.Common.Domain.GameSummary (GameSummary)
import EagerWords.Common.Domain.GameSettings (GameSettings)
import EagerWords.Common.Domain.UserDto (UserDto)
import EagerWords.Common.Domain.Piece (Piece)

-- | The api interface for the game as a type. All paths have a "game" prefix.
type GameApi =
       "base" :> "hand-shake" :> Get '[JSON] HandShakeResponse
  :<|> "base" :> "user" :> ReqBody '[JSON] UserDto :> Post '[JSON] ()
  :<|> "game" :> "game" :> ReqBody '[JSON] StartGameRequest :> Post '[JSON] StartGameResponse
  :<|> "game" :> "commit-play" :> Capture "gameId" String :> ReqBody '[JSON] [PlayPiece] :> Post '[JSON] CommitPlayResponse
  :<|> "game" :> "machine-play" :> Capture "gameId" String :> Post '[JSON] MachinePlayResponse
  :<|> "game" :> "swap-piece" :> Capture "gameId" String :> ReqBody '[JSON] Piece :> Post '[JSON] SwapPieceResponse
  :<|> "game" :> "close" :> Capture "gameId" String :> Post '[JSON] GameSummary
  :<|> "game" :> "suspend" :> Capture "gameId" String :> Post '[JSON] ()
  :<|> "game" :> "resume" :> Capture "gameId" String :> Post '[JSON] ResumeGameResponse
  :<|> "game" :> "cancel" :> Capture "gameId" String :> Post '[JSON] ()
  :<|> "game" :> "resign" :> Capture "gameId" String :> Post '[JSON] ()
  :<|> "game" :> "all" :> "user" :> Capture "userId" String :> ReqBody '[JSON] GetUserGamesRequest :> Post '[JSON] GetGamesResponse
  :<|> "game" :> "unfinished" :> "user" :> Capture "userId" String :> Get '[JSON] GetGamesResponse
  :<|> "game" :> "settings" :> "user" :> Capture "userId" String :> ReqBody '[JSON] GameSettings :> Post '[JSON] ()
  :<|> "game" :> "settings" :> "user" :> Capture "userId" String :> Get '[JSON] (Maybe GameSettings)

-- Note - In later servant versions this has changed - just use Raw.
-- Using a separate deploy for the static UI content. So this is no longer needed.
{-
type GameApi' =      GameApi
                :<|> "boardgame" :> Raw -- for index.html
                :<|> "static" :> Raw -- for js bundle
-}

-- TODO. suspendGame, resumeGame.
-- Note Capture means it is an element of the path.
-- QueryParam means it is a query parameter. The type of the query parameter is given in the API.
-- But the handler gets a Maybe of that type since the query param may not be present.

-- |The api interface for the game as a value.
gameApi :: Proxy GameApi
gameApi = Proxy

-- gameApi' :: Proxy GameApi'
-- gameApi' = Proxy


