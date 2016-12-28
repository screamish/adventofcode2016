{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}

module Advent.Day5Spec where

import Test.Hspec
import Text.InterpolatedString.Perl6 (q)

import Advent.Day5

spec :: Spec
spec = do

  describe "Day5" $ do
  --- Day 5: How About a Nice Game of Chess? ---

  -- You are faced with a security door designed by Easter Bunny engineers that seem to
  -- have acquired most of their security knowledge by watching hacking movies.

  -- The eight-character password for the door is generated one character at a time by
  -- finding the MD5 hash of some Door ID (your puzzle input) and an increasing integer
  -- index (starting with 0).

  -- A hash indicates the next character in the password if its hexadecimal representation
  -- starts with five zeroes. If it does, the sixth character in the hash is the next
  -- character of the password.

    describe "For example, if the Door ID is 'abc'" $ do
      let hashes = fiveZeroHashes "abc"

      it "The first index which produces a hash that starts with five zeroes is 3231929" $ do
        hashes !! 0 `shouldBe` ("abc3231929", "00000155f8105dff7f56ee10fa9b9abd")

      it "5017308 produces the next interesting hash, which starts with 000008f82" $ do
        hashes !! 1 `shouldBe` ("abc5017308", "000008f82c5b3924a1ecbebf60344e00")

      it "The next time a hash starts with five zeroes is for abc5278568, discovering the character f" $ do
        hashes !! 2 `shouldBe` ("abc5278568", "00000f9a2c309875e05c5a5d09f1b8c4")

      it "after continuing this search a total of eight times, the password is 18f47a30" $ do
        extractPassword (fmap snd hashes) `shouldBe` "18f47a30"

    describe "Given the actual Door ID, what is the password?" $ do
      it "Your puzzle input is cxdnnyjw." $ do
        extractPassword (fmap snd (fiveZeroHashes "cxdnnyjw")) `shouldBe` "f77a0e6e"
