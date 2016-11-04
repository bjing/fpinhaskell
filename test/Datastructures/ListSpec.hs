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

  describe "length" $ do
    it "should work for non-empty list" $ do
      length1 [1..4] `shouldBe` 4
    it "should work for empty list" $ do
      length1 [] `shouldBe` 0

  describe "foldLeft" $ do
    it "can handle large lists" $ do
      foldLeft [1..2000000] 0 (+) `shouldBe` sum [1..2000000]

  describe "sumFoldLeft" $ do
    it "should sum list items using foldLeft" $ do
      sumFoldLeft [1..2000000] `shouldBe` foldLeft [1..2000000] 0 (+)

  describe "productFoldLeft" $ do
    it "should multiply list items using foldLeft" $ do
      productFoldLeft [1..200] `shouldBe` foldLeft [1..200] 1 (*)

  describe "lengthFoldLeft" $ do
    it "should length of List using foldLeft" $ do
      productFoldLeft [1..200] `shouldBe` foldLeft [1..200] 1 (*)

  describe "reverseFold" $ do
    it "should reverse a list" $ do
      reverseFold [1..5] `shouldBe` reverse [1..5]

  describe "appendFold" $ do
    it "should append a list to another" $ do
      appendFold [1..5] [6..10] `shouldBe` [1..10]
    it "should append an empty list to another list" $ do
      appendFold [1..5] [] `shouldBe` [1..5]
    it "should append a list to an empty list" $ do
      appendFold [] [1..5] `shouldBe` [1..5]

  describe "concatViaFold" $ do
    it "should concatenate a list of lists" $ do
      concatViaFold [[1..4], [5..8], [], [9..12]] `shouldBe` [1..12]

  describe "addOne" $ do
    it "should add 1 to each item of a list" $ do
      addOne [1..4] `shouldBe` [2..5]

  describe "doubleToString" $ do
    it "should turn a list of doubles to strings" $ do
      doubleToString [1.0, 2.0, 3.0] `shouldBe` ["1.0", "2.0", "3.0"]

  describe "map" $ do
    it "should apply a function to each element of a list while maintaining the list structure" $ do
      map1 (+2) [1..5] `shouldBe` [3..7]

  describe "filter" $ do
    it "should remove all odd numbers from a list" $ do
      filter1 even [1..5] `shouldBe` [2,4]
