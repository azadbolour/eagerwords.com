--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module EagerWords.Common.Domain.GridSpec where

import Test.Hspec

import qualified Bolour.Plane.Domain.Grid as Grid

spec :: Spec
spec = do
  describe "make grid" $ do
    it "make grid" $ do
      let grid = Grid.mkGrid (\r c -> (r, c)) 2 2
      print grid






