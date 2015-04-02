{-# LANGUAGE OverloadedStrings #-}
module Cauterize.Crucible.PrototypesSpec
  ( spec
  ) where

import Test.Hspec

import Cauterize.Crucible.Prototypes

spec :: Spec
spec = do
  describe "parsePrototypeVariant" $
    it "is able to parse all prototypes" $ do
      parsePrototypeVariant "synonym" `shouldBe` Just PVSynonym
      parsePrototypeVariant "array" `shouldBe` Just PVArray
      parsePrototypeVariant "vector" `shouldBe` Just PVVector
      parsePrototypeVariant "record" `shouldBe` Just PVRecord
      parsePrototypeVariant "combination" `shouldBe` Just PVCombination
      parsePrototypeVariant "union" `shouldBe` Just PVUnion
  describe "parsePrototypeVariants" $
    it "is able to parse comma-separated lists of prototypes" $ do
      parsePrototypeVariants "array,vector" `shouldBe` Just [PVArray, PVVector]
      parsePrototypeVariants "array,union" `shouldBe` Just [PVArray, PVUnion]
      parsePrototypeVariants "array,union,vector" `shouldBe` Just [PVArray, PVUnion, PVVector]
      parsePrototypeVariants "synonym,array,vector,record,combination,union"
        `shouldBe` Just [PVSynonym,PVArray,PVVector,PVRecord,PVCombination,PVUnion]
