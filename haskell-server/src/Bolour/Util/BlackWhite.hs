--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module Bolour.Util.BlackWhite (
    BlackWhite(..)
  , isJustWhite
  , fromWhite
  , fromWhites
  , isWhite
  , isBlack
  , hasValue
  , toValueWithDefaults
  )
  where

import Data.Maybe (isNothing, isJust, fromJust)
import qualified Bolour.Util.Empty as Empty

-- | Black represents a value in an inactive or disabled location, for example,
--   the black square of a crossword puzzle; white represents a
--   value in a live location, which may be empty at the moment (represented
--   as Maybe).
data BlackWhite val = Black | White (Maybe val)
  deriving Functor

deriving instance (Eq val) => Eq (BlackWhite val)
deriving instance (Show val) => Show (BlackWhite val)

toValueWithDefaults :: BlackWhite val -> val -> val -> val
toValueWithDefaults bw blackDefault whiteDefault | isBlack bw = blackDefault
                           | Empty.isEmpty bw = whiteDefault
                           | otherwise = fromJust $ fromWhite bw

isJustWhite :: BlackWhite val -> Bool
isJustWhite Black = False
isJustWhite (White Nothing) = False
isJustWhite _ = True

fromWhite :: BlackWhite val -> Maybe val
fromWhite Black = Nothing
fromWhite (White Nothing) = Nothing
fromWhite (White x) = x

fromWhites :: [BlackWhite val] -> Int -> Int -> [Maybe val]
fromWhites line begin end =
  toMaybe <$> [begin .. end]
    where toMaybe i = fromWhite (line !! i)

instance Empty.Empty (BlackWhite val)
  where isEmpty bw =
          case bw of
          Black -> False
          White maybe -> isNothing maybe

isWhite :: BlackWhite val -> Bool
isWhite (White _) = True
isWhite Black = False

isBlack :: BlackWhite val -> Bool
isBlack (White _) = False
isBlack Black = True

hasValue :: BlackWhite val -> Bool
hasValue (White value) = isJust value
hasValue Black = False







