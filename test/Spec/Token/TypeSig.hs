module Spec.Token.TypeSig (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)



spec :: IO ()
spec = hspec $ do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)
