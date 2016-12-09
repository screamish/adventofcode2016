module Advent.Day1Spec where

import Test.Hspec

import Advent.Day1

spec :: Spec
spec = do
-- You're airdropped near Easter Bunny Headquarters in a city somewhere. "Near",
-- unfortunately, is as close as you can get - the instructions on the Easter Bunny
-- Recruiting Document the Elves intercepted start here, and nobody had time to work them
-- out further.

-- The Document indicates that you should start at the given coordinates (where you just
-- landed) and face North. Then, follow the provided sequence: either turn left (L) or
-- right (R) 90 degrees, then walk forward the given number of blocks, ending at a new
-- intersection.

-- There's no time to follow such ridiculous instructions on foot, though, so you take a
-- moment and work out the destination. Given that you can only walk on the street grid of
-- the city, how far is the shortest path to the destination?

  describe "Day1" $ do
    describe "Following R2, L3 leaves you 2 blocks East and 3 blocks North, or 5 blocks away." $ do
      it "parses" $ do
        parseMovements "R2, L3" `shouldBe` [(R, 2), (L, 3)]
      it "computes" $ do
        bunny (parseMovements "R2, L3") `shouldBe` (2, 3)
    describe "R2, R2, R2 leaves you 2 blocks due South of your starting position, which is 2 blocks away." $ do
      it "parses" $ do
        parseMovements "R2, R2, R2" `shouldBe` [(R, 2), (R, 2), (R, 2)]
      it "computes" $ do
        bunny (parseMovements "R2, R2, R2") `shouldBe` (0, -2)
    describe "R5, L5, R5, R3 leaves you 12 blocks away." $ do
      it "parses" $ do
        parseMovements "R5, L5, R5, R3" `shouldBe` [(R, 5), (L, 5), (R,5), (R,3)]
      it "computes" $ do
        bunny (parseMovements "R5, L5, R5, R3") `shouldBe` (10, 2)

--- Part Two ---

-- Then, you notice the instructions continue on the back of the Recruiting Document. Easter Bunny HQ is actually at the first location you visit twice.

-- For example, if your instructions are R8, R4, R4, R8, the first location you visit twice is 4 blocks away, due East.

-- How many blocks away is the first location you visit twice?
    describe "Part 2 - first to visit twice" $ do
      it "if your instructions are R8, R4, R4, R8, the first location you visit twice is 4 blocks away, due East." $ do
        bunny (parseMovements "R8, R4, R4, R8") `shouldBe` (4,0)
