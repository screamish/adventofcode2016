{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}

module Advent.Day4Spec where

import Test.Hspec
import Text.InterpolatedString.Perl6 (q)

import Advent.Day4

spec :: Spec
spec = do

--- Day 4: Security Through Obscurity ---

-- Finally, you come across an information kiosk with a list of rooms. Of course, the list
-- is encrypted and full of decoy data, but the instructions to decode the list are barely
-- hidden nearby. Better remove the decoy data first.

  describe "Day4" $ do
    describe "Room names" $ do
    -- Each room consists of an encrypted name (lowercase letters separated by dashes)
    -- followed by a dash, a sector ID, and a checksum in square brackets.
      it "can parse a Room" $ do
        parse' room "nzydfxpc-rclop-qwzhpc-qtylyntyr-769[oshgk]" `shouldBe`
          Right (Room "nzydfxpc-rclop-qwzhpc-qtylyntyr" 769 "oshgk")

      it "can parse Rooms" $ do
        parse' rooms [q|aaaaa-bbb-z-y-x-123[abxyz]
a-b-c-d-e-f-g-h-987[abcde]
not-a-real-room-404[oarel]
totally-real-room-200[decoy]|] `shouldBe` Right [
            Room "aaaaa-bbb-z-y-x" 123 "abxyz"
          , Room "a-b-c-d-e-f-g-h" 987 "abcde"
          , Room "not-a-real-room" 404 "oarel"
          , Room "totally-real-room" 200 "decoy"
          ]

    -- A room is real (not a decoy) if the checksum is the five most common letters in the
    -- encrypted name, in order, with ties broken by alphabetization. For example:

      it "aaaaa-bbb-z-y-x-123[abxyz] is a real room because the most common letters are a (5), b (3), and then a tie between x, y, and z, which are listed alphabetically." $ do
        checksum "aaaaa-bbb-z-y-x" `shouldBe` "abxyz"
        isReal (Room "aaaaa-bbb-z-y-x" 123 "abxyz") `shouldBe` True

      it "a-b-c-d-e-f-g-h-987[abcde] is a real room because although the letters are all tied (1 of each), the first five are listed alphabetically." $ do
        checksum "a-b-c-d-e-f-g-h" `shouldBe` "abcde"
        isReal (Room "a-b-c-d-e-f-g-h" 987 "abcde") `shouldBe` True

      it "not-a-real-room-404[oarel] is a real room." $ do
        checksum "not-a-real-room" `shouldBe` "oarel"
        isReal (Room "not-a-real-room" 404 "oarel") `shouldBe` True

      it "totally-real-room-200[decoy] is not." $ do
        checksum "aaaaa-bbb-z-y-x" `shouldBe` "abxyz"
        isReal (Room "totally-real-room" 200 "decoy") `shouldBe` False

      it "Of the real rooms from the list above, the sum of their sector IDs is 1514." $ do
        sectorIdSum
          [q|aaaaa-bbb-z-y-x-123[abxyz]
a-b-c-d-e-f-g-h-987[abcde]
not-a-real-room-404[oarel]
totally-real-room-200[decoy]|] `shouldBe` 1514

    describe "Part B" $ do
    -- With all the decoy data out of the way, it's time to decrypt this list and get
    -- moving.

    -- The room names are encrypted by a state-of-the-art shift cipher, which is nearly
    -- unbreakable without the right software. However, the information kiosk designers at
    -- Easter Bunny HQ were not expecting to deal with a master cryptographer like
    -- yourself.

      describe "decryption" $ do
      -- To decrypt a room name, rotate each letter forward through the alphabet a number
      -- of times equal to the room's sector ID. A becomes B, B becomes C, Z becomes A,
      -- and so on. Dashes become spaces.

        it "qzmt-zixmtkozy-ivhz-343 is 'very encrypted name'" $ do
          decrypt "qzmt-zixmtkozy-ivhz" 343 `shouldBe`  "very encrypted name"
