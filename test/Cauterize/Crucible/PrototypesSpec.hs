{-# LANGUAGE OverloadedStrings #-}
module Cauterize.Crucible.PrototypesSpec
  ( spec
  ) where

import Test.Hspec

import Cauterize.Crucible.Prototypes

spec :: Spec
spec =
  describe "parsePrototypeVariant" $
    it "is able to parse all prototypes" $ do
      parsePrototypeVariant "synonym" `shouldBe` Just PVSynonym
      parsePrototypeVariant "array" `shouldBe` Just PVArray
      parsePrototypeVariant "vector" `shouldBe` Just PVVector
      parsePrototypeVariant "record" `shouldBe` Just PVRecord
      parsePrototypeVariant "combination" `shouldBe` Just PVCombination
      parsePrototypeVariant "union" `shouldBe` Just PVUnion
