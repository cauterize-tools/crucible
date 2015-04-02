{-# LANGUAGE OverloadedStrings #-}
module Cauterize.Crucible.Prototypes
  ( PrototypeVariant(..)
  , parsePrototypeVariant
  , parsePrototypeVariants
  , allPrototypeVariants
  ) where

import qualified Data.Text as T
import Cauterize.Generate

parsePrototypeVariant :: Monad m => T.Text -> m PrototypeVariant
parsePrototypeVariant "synonym" = return PVSynonym
parsePrototypeVariant "array" = return PVArray
parsePrototypeVariant "vector" = return PVVector
parsePrototypeVariant "record" = return PVRecord
parsePrototypeVariant "combination" = return PVCombination
parsePrototypeVariant "union" = return PVUnion
parsePrototypeVariant s = fail $ T.unpack s

parsePrototypeVariants :: Monad m => T.Text -> m [PrototypeVariant]
parsePrototypeVariants s = do
  let protStrs = T.splitOn "," s
  mapM parsePrototypeVariant protStrs

allPrototypeVariants:: [PrototypeVariant]
allPrototypeVariants = [minBound..maxBound]
