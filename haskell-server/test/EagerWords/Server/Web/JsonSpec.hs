--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

module EagerWords.Server.Web.JsonSpec (
    spec
  ) where

import Test.Hspec
import Data.Aeson
import Control.Monad.Trans.Except (runExceptT)

import EagerWords.Common.Domain.InitPieces (InitPieces(InitPieces))
import EagerWords.Common.Domain.GameParams (GameParams, GameParams(GameParams))
import EagerWords.Common.Domain.Piece (Piece)
import Bolour.Plane.Domain.Point (Point, Point(Point))
import qualified EagerWords.Common.Domain.Piece as Piece
import EagerWords.Server.Domain.GameError
import qualified Bolour.Util.SpecUtil as SpecUtil
import qualified Bolour.Language.Domain.WordDictionary as Dict
import qualified EagerWords.Server.Domain.PieceProvider as PieceProvider
import qualified EagerWords.Common.Domain.PieceProviderType as PieceProviderType
import EagerWords.Common.Domain.GameSettings (GameSettings, GameSettings(GameSettings))
import qualified EagerWords.Common.Domain.GameSettings as GameSettings

spec :: Spec

name = "You"
dim = 9

pointValues :: [[Int]]
pointValues = replicate dim $ replicate dim (1 :: Int)

testPieceProviderType = PieceProviderType.Cyclic
params :: GameParams
params = GameParams (GameSettings dim 50 12 Dict.defaultLanguageCode testPieceProviderType Nothing Nothing) pointValues
pieceProvider = PieceProvider.mkDefaultCyclicPieceProvider

initPieces = InitPieces [] [] []

-- game :: IO Game
-- game =
--   SpecUtil.satisfiesRight =<< runExceptT (Game.mkInitialGame params initPieces pieceProvider pointValues player)

spec =
  describe "json for game data" $ do
    it "convert game params to/from json" $ do
      let piece = Piece.Piece 'T' "1"
          encoded = encode (params, [piece])
      print encoded
      let decoded = decode encoded :: Maybe (GameParams, [Piece])
      print decoded
    it "convert a game error to/from json" $ do
      let error = NonContiguousPlayError $ [Point 1 1]
      let encoded = encode error
      print encoded
      let decoded = decode encoded :: Maybe GameError
      print decoded




