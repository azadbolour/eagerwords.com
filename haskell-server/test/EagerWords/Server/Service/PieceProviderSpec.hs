--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

module EagerWords.Server.Service.PieceProviderSpec where

import Test.Hspec
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Except (ExceptT, runExceptT)
import Data.List

import EagerWords.Common.Domain.Piece (Piece, Piece(Piece))
import qualified EagerWords.Common.Domain.Piece as Piece
import Bolour.Util.FrequencyDistribution (FrequencyDistribution(..))
import qualified Bolour.Util.FrequencyDistribution as FrequencyDistribution
import qualified EagerWords.Server.Domain.PieceProvider as PieceProvider
import EagerWords.Server.Domain.PieceProvider (PieceProvider(..))
import EagerWords.Server.Domain.GameError (GameError)

letterFrequencies = [
    ('A', 10),
    ('B', 20),
    ('C', 30),
    ('D', 40)
  ]

letterDistribution :: FrequencyDistribution Char
letterDistribution = FrequencyDistribution.mkFrequencyDistribution letterFrequencies

provider0 :: PieceProvider
provider0 = RandomPieceProvider 0 (FrequencyDistribution.randomValue letterDistribution)

makePiece :: PieceProvider -> IO (Piece, PieceProvider)
makePiece provider = PieceProvider.take provider

nextState :: IO (Piece, PieceProvider) -> IO (Piece, PieceProvider)
nextState ioPair = do
  (piece, provider) <- ioPair
  makePiece provider

spec :: Spec
spec =
  describe "for visual inspection of random piece generation" $
    it "generate random pieces and count the letters generated" $ do
      (piece, provider) <- PieceProvider.take provider0
      pairs <- sequence $ take 100 $ iterate nextState (return (piece, provider))
      let pieces = fst <$> pairs
          letters = Piece.value <$> pieces
      print letters
      let groups = group $ sort letters
          counted = (\g -> (head g, length g)) <$> groups
      print counted





