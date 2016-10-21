module Datastructures.ListSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Datastructures.List

main :: IO ()
main = hspec $ spec

spec :: Spec
spec = do
  describe "tail" $ do
    it "should return tail" $ do
      tail1 [1..5] `shouldBe` [2..5]
    it "should return tail of an empty list" $ do
      tail1 [] `shouldBe` ([] :: [Int])

  describe "setHead" $ do
    it "should set new head for list" $ do
      setHead [1..3] 4 `shouldBe` [4, 1, 2, 3]

  describe "drop" $ do
    it "should drop for empty list" $ do
      drop1 [] 2 `shouldBe` ([] :: [Int])
    it "should drop 2 elements from list" $ do
      drop1 [1..4] 2 `shouldBe` [3,4]

  describe "dropWhile" $ do
    it "should work for empty list" $ do
      dropWhile1 [] (const True) `shouldBe` ([] :: [Int])
    it "should drop element while condition remains true" $ do
      dropWhile1 [1..5] (< 3) `shouldBe` [3, 4, 5]

  describe "init" $ do
    it "should work for empty list" $ do
      init1 [] `shouldBe` ([] :: [Int])
    it "should work for single item list" $ do
      init1 [1] `shouldBe` []
    it "" $ do
      init1 [1..5] `shouldBe` [1..4]

  describe "product" $ do
    it "should calculate product for non-empty list" $ do
      product1 [1..4] `shouldBe` 24
    it "should handle empty list" $ do
      product1 [] `shouldBe` 1

  describe "" $ do
    it "should work for non-empty list" $ do
      length1 [1..4] `shouldBe` 4
    it "should work for empty list" $ do
      length1 [] `shouldBe` 0
