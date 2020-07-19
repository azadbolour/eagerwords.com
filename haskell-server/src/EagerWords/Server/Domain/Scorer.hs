--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- TODO. Create tests for the scorer module.
-- Pay particular attention to representation of empty cells in the implementation.
-- Then connect this module to the rest of the application and use it to compute scores.
module EagerWords.Server.Domain.Scorer (
    Scorer
  , Scorer(scorePlay, scoreWord)
  , mkScorer
  ) where

import qualified Data.List as List
import Bolour.Plane.Domain.Point (Point, Point(Point))
import qualified Bolour.Plane.Domain.Point as Point
import EagerWords.Common.Domain.PlayPiece (PlayPiece, PlayPiece(PlayPiece), MoveInfo)
import qualified EagerWords.Common.Domain.PlayPiece as PlayPiece

data Scorer = Scorer {
    scorePlay :: [PlayPiece] -> Int
  , scoreWord :: [PlayPiece] -> Int
}

mkScorer :: [[Int]] -> Scorer
mkScorer pointValues =
  Scorer
       (doScoreWord pointValues)
       (doScoreWord pointValues)

doScoreWord :: [[Int]] -> [PlayPiece] -> Int
doScoreWord pointValues playPieces =
  let movedPoints = PlayPiece.point <$> List.filter PlayPiece.moved playPieces
      value Point {row, col} = (pointValues !! row) !! col
      accumulate total point = total + value point
  in List.foldl' accumulate 0 movedPoints

