--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module EagerWords.Util.TestUtil (
  mkInitialPlayPieces
  ) where

import EagerWords.Common.Domain.Piece (Piece)
import EagerWords.Common.Domain.PlayPiece (PlayPiece, PlayPiece(PlayPiece))
import Bolour.Plane.Domain.Point (Point, Point(Point))
import qualified Bolour.Plane.Domain.Point as Point
import EagerWords.Common.Domain.PiecePoint (PiecePoint, PiecePoint(PiecePoint))
import qualified EagerWords.Common.Domain.PiecePoint as PiecePoint

mkInitialPlayPieces :: PiecePoint -> [Piece] -> [PlayPiece]
mkInitialPlayPieces (centerBoardPiece @ PiecePoint {piece, point}) trayPieces = playPieces where
  Point.Point {row, col} = point
  -- r = row $ point
  -- c = col $ point
  centerPlayPiece = PlayPiece piece point False
  leftPiece = trayPieces !! 0
  leftPlayPiece = PlayPiece leftPiece (Point row (col - 1)) True
  rightPiece = trayPieces !! 1
  rightPlayPiece = PlayPiece rightPiece (Point row (col + 1)) True
  playPieces = [leftPlayPiece, centerPlayPiece, rightPlayPiece]


