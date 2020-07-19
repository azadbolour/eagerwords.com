--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--


-- | Special hspec main to run only the tests that are currently in focus.
--   See test-suite 'infocus' in .cabal file. Add a 'describe' here for each
--   in-focus test. And add the test module to the infocus test-suite in .cabal.
module Main (
    main
  , spec
  ) where

-- TODO. stack test discovers this as well - and runs it redundantly.
-- TODO. Don't know how to fix. Ignore for now.

import Test.Hspec

import qualified EagerWords.Server.Domain.StripMatcherSpec3 as Test1
import qualified EagerWords.Server.Domain.HopelessBlanksSpec2 as Test2
import qualified Bolour.Language.Domain.WordDictionarySpec as Test3

main :: IO ()
main = do
    print "infocus tests"
    hspec spec

spec :: Spec
spec = do
  describe "Test1" Test1.spec
  -- describe "Test2" Test2.spec
  -- describe "Test3" Test3.spec
  -- describe "Test4" Test4.spec