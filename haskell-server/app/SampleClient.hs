module Main where

-- import Control.Monad.Except (runExceptT)

import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Servant.Client
-- import Servant.Common.BaseUrl()

import EagerWords.Common.Domain.UserDto (UserDto(UserDto))
import EagerWords.Common.Domain.Piece (Piece)
import EagerWords.Common.Domain.InitPieces (InitPieces(InitPieces))
import qualified EagerWords.Common.Domain.Piece as Piece
-- import Bolour.Plane.Domain.Point (Point(Point))
-- import qualified Bolour.Plane.Domain.Point as Point
import EagerWords.Common.Domain.GameParams (GameParams(GameParams))
-- import qualified EagerWords.Common.Domain.Piece as Piece
-- import qualified EagerWords.Common.Domain.GameParams as GameParams
import qualified EagerWords.Client.GameClient as Client
import EagerWords.Common.Message.StartGameRequest (StartGameRequest(StartGameRequest))
import qualified EagerWords.Common.Domain.PieceProviderType as PieceProviderType
import EagerWords.Common.Domain.GameSettings (GameSettings, GameSettings(GameSettings))
import qualified EagerWords.Common.Domain.GameSettings as GameSettings

dimension = 9
userId = "222255555"
name = "John"
player = UserDto userId name (name ++ "@example.com")
pieceGeneratorType = PieceProviderType.Cyclic
pointValues :: [[Int]]
pointValues = replicate dimension $ replicate dimension (1 :: Int)
gameParams = GameParams (GameSettings dimension 12 50 "en" pieceGeneratorType Nothing Nothing) pointValues 

mkPiece :: Char -> Int -> Piece
mkPiece letter id = Piece.Piece letter (show id)

main :: IO ()
main = do
  let baseUrl = BaseUrl Http "localhost" 6587 ""
  manager <- mkManager
  -- eitherMaybeUnit <- runExceptT (Client.addPlayer player manager baseUrl)
  eitherMaybeUnit <- runClientM (Client.addUser player) (mkClientEnv manager baseUrl)
  print $ show eitherMaybeUnit

  -- Ideally should get pieces from the server - since the server is the owner of the tile sack.
  -- We'll let that go for now.

  let uPieces = [mkPiece 'B' 1, mkPiece 'E' 2, mkPiece 'T' 3] -- Allow the word 'BET'
      mPieces = [mkPiece 'S' 4, mkPiece 'E' 5, mkPiece 'Z' 6] -- Allow the word 'SET' across.
      initPieces = InitPieces [] uPieces mPieces

  eitherGameDto <- runClientM (Client.startGame (StartGameRequest gameParams initPieces userId)) (mkClientEnv manager baseUrl)
  print $ show eitherGameDto
  print ""

mkManager :: IO Manager
mkManager = newManager defaultManagerSettings
