{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}

module Advent.Day3Spec where

import Test.Hspec
import Text.InterpolatedString.Perl6 (q)

import Advent.Day3

spec :: Spec
spec = do

--- Day 3: Squares With Three Sides ---
-- Now that you can think clearly, you move deeper into the labyrinth of hallways and
-- office furniture that makes up this part of Easter Bunny HQ. This must be a graphic
-- design department; the walls are covered in specifications for triangles.

-- Or are they?

-- The design document gives the side lengths of each triangle it describes, but... 5 10
-- 25? Some of these aren't triangles. You can't help but mark the impossible ones.

-- In a valid triangle, the sum of any two sides must be larger than the remaining side.
-- For example, the "triangle" given above is impossible, because 5 + 10 is not larger
-- than 25.

  describe "Day3" $ do
    describe "Parsing triangles" $ do
      it "can parse" $ do
        parseTriangles "541  588  421 \n 827  272  126" `shouldBe` [ (541, 588, 421)
                                                                   , (827, 272, 126) ]

    describe "Triangle with sides 5 10 25" $ do
      it "is impossible" $ do
        isTriangle 5 10 25 `shouldBe` False

    -- Now that you've helpfully marked up their design documents, it occurs to you that
    -- triangles are specified in groups of three vertically. Each set of three numbers in a
    -- column specifies a triangle. Rows are unrelated.

    -- For example, given the following specification, numbers with the same hundreds digit
    -- would be part of the same triangle:
    describe "Parsing vertically" $ do
      it "can transpose" $ do
        transposeTriangles [(101, 301, 501)
                            ,(102, 302, 502)
                            ,(103, 303, 503)
                            ,(201, 401, 601)
                            ,(202, 402, 602)
                            ,(203, 403, 603)] `shouldBe` [(101, 102, 103)
                                                          ,(301, 302, 303)
                                                          ,(501, 502, 503)
                                                          ,(201, 202, 203)
                                                          ,(401, 402, 403)
                                                          ,(601, 602, 603)]
      it "can parse" $ do
        parseTriangles [q| 101 301 501
                           102 302 502
                           103 303 503
                           201 401 601
                           202 402 602
                           203 403 603 |] `shouldBe` [(101, 301, 501)
                                                      ,(102, 302, 502)
                                                      ,(103, 303, 503)
                                                      ,(201, 401, 601)
                                                      ,(202, 402, 602)
                                                      ,(203, 403, 603)]
