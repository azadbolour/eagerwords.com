
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.Maybe as Maybe

import qualified Data.Text as Text
import Data.ByteString.Lazy.Char8 as BS

import qualified Data.HashMap.Strict as HashMap

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.Aeson (encode, decode, toJSON, Value, Value(Object))

import EagerWords.Server.Domain.GameError (GameError(..))
import qualified EagerWords.Server.Domain.GameError as GameError

main :: IO ()

data TestUnion = One | Two 
  deriving (Eq, Show, Generic)

instance FromJSON TestUnion
instance ToJSON TestUnion

main = do
  let encoded = GameError.encodeGameErrorWithMessage $ GameTimedOutError "000000" 20
  print encoded
  let encoded' = GameError.encodeGameErrorWithMessage $ InternalError "ERRRRR"
  print encoded'
  let nothing :: Maybe Int
      nothing = Nothing
      encodedNothing = encode nothing
  print encodedNothing
  print $ BS.length encodedNothing -- prints 4
  print ("encoded Nothing: " ++ (show $ encodedNothing)) -- prints "null"
  let decodedNothing :: Maybe Int
      decodedNothing = decode encodedNothing
  print ("decoded nothing: " ++ (show decodedNothing))
  
  let just :: Maybe Int
      just = Just 100
      encodedJust = encode just
  print ("encoded Just: " ++ (show encodedJust)) -- will print "100"
  let decodedJust :: Maybe Int
      decodedJust = decode encodedJust
  print ("decoded just: " ++ (show decodedJust))
  
  let union :: Maybe TestUnion
      union = Just One
      encodedUnion = encode union
  print ("encoded Union: " ++ (show encodedUnion)) -- 
  let decodedUnion :: Maybe TestUnion
      decodedUnion = decode encodedUnion
  print ("decoded union: " ++ (show decodedUnion))
  
  print "OK"

