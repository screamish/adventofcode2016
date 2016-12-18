{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}

module Advent.Day2Spec where

import Test.Hspec
import Text.InterpolatedString.Perl6 (q)
import Data.Map.Strict

import Advent.Day2

spec :: Spec
spec = do
--- Day 2: Bathroom Security ---

-- You arrive at Easter Bunny Headquarters under cover of darkness. However, you left in
-- such a rush that you forgot to use the bathroom! Fancy office buildings like this one
-- usually have keypad locks on their bathrooms, so you search the front desk for the
-- code.

-- "In order to improve security," the document you find says, "bathroom codes will no
-- longer be written down. Instead, please memorize and follow the procedure below to
-- access the bathrooms."

-- The document goes on to explain that each button to be pressed can be found by starting
-- on the previous button and moving to adjacent buttons on the keypad: U moves up, D
-- moves down, L moves left, and R moves right. Each line of instructions corresponds to
-- one button, starting at the previous button (or, for the first line, the "5" button);
-- press whatever button you're on at the end of each line. If a move doesn't lead to a
-- button, ignore it.

-- You can't hold it much longer, so you decide to figure out the code as you walk to the
-- bathroom. You picture a keypad like this:

-- 1 2 3
-- 4 5 6
-- 7 8 9

  describe "Day2" $ do
    let sample = [q|ULL
RRDDD
LURDL
UUUUD|]

    describe "Parsing" $ do
      it "ULL is [U, L, L]" $ do
        parseMovements "ULL" `shouldBe` [[U, L, L]]
      it "can parse multilines" $ do
        parseMovements "ULL\nRRDDD" `shouldBe` [[U,L,L],[R,R,D,D,D]]

    describe "Movement tests" $ do
      let sut key movement = _keys squareKeypad ! move squareKeypad key movement
      it "Up    from 5 is 2"  $ do sut (2,2) U  `shouldBe` "2"
      it "Down  from 5 is 8"  $ do sut (2,2) D  `shouldBe` "8"
      it "Left  from 5 is 4"  $ do sut (2,2) L  `shouldBe` "4"
      it "Right from 5 is 6"  $ do sut (2,2) R  `shouldBe` "6"
      it "Down  from 8 is 8"  $ do sut (3,2) D  `shouldBe` "8"
      it "Right from 6 is 6"  $ do sut (2,3) R  `shouldBe` "6"

    it "The example given evaluates to 1985" $ do
      -- Suppose your instructions are:

      -- ULL
      -- RRDDD
      -- LURDL
      -- UUUUD

      -- You start at "5" and move up (to "2"), left (to "1"), and left (you can't, and
      -- stay on "1"), so the first button is 1. Starting from the previous button ("1"),
      -- you move right twice (to "3") and then down three times (stopping at "9" after
      -- two moves and ignoring the third), ending up with 9. Continuing from "9", you
      -- move left, up, right, down, and left, ending with 8. Finally, you move up four
      -- times (stopping at "2"), then down once, ending with 5. So, in this example, the
      -- bathroom code is 1985.
      decodePIN squareKeypad sample `shouldBe` "1985"

    describe "Part b" $ do
      it "parses the same input to 5DB3" $ do
        decodePIN diamondKeypad sample `shouldBe` "5DB3"
