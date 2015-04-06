{-# LANGUAGE OverloadedStrings #-}
module Cauterize.Crucible.Prototypes
  ( PrototypeVariant(..)
  , parsePrototypeVariant
  , parsePrototypeVariants
  , protoVarToStr
  , allPrototypeVariants
  ) where

import qualified Data.Text.Lazy as T
import Cauterize.Generate
import Data.String

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

protoVarToStr :: IsString a => PrototypeVariant -> a
protoVarToStr PVSynonym     = "synonym"
protoVarToStr PVArray       = "array"
protoVarToStr PVVector      = "vector"
protoVarToStr PVRecord      = "record"
protoVarToStr PVCombination = "combination"
protoVarToStr PVUnion       = "union"

allPrototypeVariants:: [PrototypeVariant]
allPrototypeVariants = [minBound..maxBound]
