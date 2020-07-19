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
{-# LANGUAGE ScopedTypeVariables #-}

module EagerWords.Server.Domain.PieceProvider (
    PieceProvider(..)
  , EagerWords.Server.Domain.PieceProvider.take
  , takePieces
  , swapOne
  , pieceProviderType
  , mkDefaultCyclicPieceProvider
  )
  where

import EagerWords.Common.Domain.Piece (Piece, Piece(Piece))
import qualified EagerWords.Common.Domain.PieceProviderType as PieceProviderType
import EagerWords.Common.Domain.PieceProviderType
import EagerWords.Server.Domain.GameError (GameError)

-- The piece generator types are closed in this implementation.
-- TODO. Would be nice to have an open piece generator implementation model.
-- Had some type system issues using type classes.

-- TODO. Name cyclic constructor parameters.

-- | Piece generator.
--   Included in the common package to allow client tests
--   to generate pieces consistently with the server.
data PieceProvider =
  RandomPieceProvider { counter :: Integer, randomizer :: IO Char} |
  CyclicPieceProvider Integer String

take :: PieceProvider -> IO (Piece, PieceProvider)

take (provider @ RandomPieceProvider {counter, randomizer}) = do
  letter <- randomizer
  let piece = Piece letter (show counter)
  let nextProvider = RandomPieceProvider (counter + 1) randomizer
  return (piece, nextProvider)

take (CyclicPieceProvider count cycler) = do
  let count' = count + 1
      piece = Piece (head cycler) (show count')
  return (piece, CyclicPieceProvider count' (drop 1 cycler))

-- TODO. Best practices to disambiguate against Prelude [take]?
take' :: PieceProvider -> IO (Piece, PieceProvider)
take' = EagerWords.Server.Domain.PieceProvider.take

takePieces :: PieceProvider -> Int -> IO ([Piece], PieceProvider)
takePieces provider max = takePiecesAux provider [] max

takePiecesAux :: PieceProvider -> [Piece] -> Int -> IO ([Piece], PieceProvider)
takePiecesAux provider list n =
  if n <= 0
    then return (list, provider)
    else do
      (piece, provider1) <- take' provider
      (pieces, provider2) <- takePiecesAux provider1 (piece:list) (n - 1)
      return (pieces, provider2)

swapOne :: PieceProvider -> Piece -> IO (Piece, PieceProvider)
swapOne provider piece = do
  (swappedPiece, provider1) <- take' provider
  -- provider2 <- give provider1 piece
  return (swappedPiece, provider1)

pieceProviderType :: PieceProvider -> PieceProviderType
pieceProviderType (RandomPieceProvider _ _) = PieceProviderType.Random
pieceProviderType (CyclicPieceProvider _ _) = PieceProviderType.Cyclic

caps = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
mkDefaultCyclicPieceProvider :: PieceProvider
mkDefaultCyclicPieceProvider = CyclicPieceProvider 0 (cycle caps)
