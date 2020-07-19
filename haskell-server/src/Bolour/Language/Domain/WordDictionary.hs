--
-- Copyright 2017-2020 Azad Bolour
-- Licensed under GNU Affero General Public License v3.0 -
--   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
--

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Bolour.Language.Domain.WordDictionary (
    WordDictionary(WordDictionary, languageCode, maxMaskedLetters)
  , mkDictionary
  , mkMaskedWords
  , defaultLanguageCode
  , isWord
  , isMaskedWord
  , getWordPermutations
  , getAllWords
  ) where

import Data.Bool (bool)
import qualified Data.Char as Char
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy.Char8 as BS

import qualified Bolour.Util.MiscUtil as MiscUtil
import Bolour.Language.Util.WordUtil (LetterCombo, WordIndex, DictWord)
import qualified Bolour.Language.Util.WordUtil as WordUtil

english :: String
english = "en"

defaultLanguageCode :: String
defaultLanguageCode = english

-- | A dictionary of words in a given language (identified by language code).
data WordDictionary = WordDictionary {
    languageCode :: String  -- ^ public
  , words :: Set String     -- ^ private
  , index :: WordIndex      -- ^ private
  , maskedWords :: Set BS.ByteString
  , maxMaskedLetters :: Int
}

mkDictionary :: String -> [DictWord] -> [BS.ByteString] -> Int -> WordDictionary
mkDictionary languageCode words maskedWords maxMaskedLetters =
  let wordSet = Set.fromList words
      wordIndex = mkWordIndex words
      maskedWordSet = Set.fromList maskedWords
  in WordDictionary languageCode wordSet wordIndex maskedWordSet maxMaskedLetters

mkMaskedWords :: [DictWord] -> Int -> Set BS.ByteString
mkMaskedWords words maxMaskedLetters =
  let list = do
       word <- words
       masked <- WordUtil.maskWithBlanks word maxMaskedLetters
       return $ BS.pack masked
  in Set.fromList list

-- | Return all words of the dictionary as a set.
getAllWords :: WordDictionary -> Set String
getAllWords WordDictionary {words} = words

-- | True iff given string represents a word.
isWord :: WordDictionary -> String -> Bool
isWord WordDictionary {words} word = word `Set.member` words

isMaskedWord :: WordDictionary -> String -> Bool
isMaskedWord WordDictionary {maskedWords} masked =
  let rawMasked = BS.pack masked
  in rawMasked `Set.member` maskedWords

-- | Look up the words that are permutations of a given combination of letters.
getWordPermutations ::
     WordDictionary -- ^ The dictionary.
  -> LetterCombo -- ^ Combinations of letters represented as a SORTED byte string.
  -> [DictWord]  -- ^ Permutations of the given combination that are in the dictionary.
getWordPermutations (dictionary @ WordDictionary {index}) combo =
  Maybe.fromMaybe [] (Map.lookup combo index)

-- Private functions.

mkWordIndex :: [DictWord] -> WordIndex
mkWordIndex words = MiscUtil.mapFromValueList WordUtil.mkLetterCombo words



