{-# LANGUAGE OverloadedStrings #-}
module Token.TypeSigSpec (spec) where

import Test.Hspec

import Token.TypeSig


spec :: Spec
spec =
  describe "toTokens" $ do
    it "Symbols are split in to groups" $
      toList (toTokens "Maybe a") `shouldBe` [("Maybe", 1), ("a", 1)]

    it "Symbols are counted" $
      toList (toTokens "Maybe a -> Maybe b")
        `shouldBe` [("->", 1), ("Maybe", 2), ("a", 1), ("b", 1)]

    it "Symbols are simplified" $
      toList (toTokens "Maybe foo -> Maybe bar")
        `shouldBe` [("->", 1), ("Maybe", 2), ("a", 1), ("b", 1)]

    it "Parens are ignored" $
      toList (toTokens "Int -> (String -> Int)")
        `shouldBe` [("->", 2), ("Int", 2), ("String", 1)]

    it "Curlybraces are ignored" $
      toList (toTokens "{foo : Int}")
        `shouldBe` [(":", 1), ("Int", 1), ("a", 1)]
