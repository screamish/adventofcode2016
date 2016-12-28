{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase #-}
module Advent.Day5 where

import Control.Arrow ((&&&))
import qualified Data.ByteArray as BA
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Maybe
import Data.Monoid
import qualified Data.ByteString.Base16 as B16
import Data.Char (isHexDigit, digitToInt)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Crypto.Hash.Algorithms (MD5(..))
import           Crypto.Hash (hashWith)

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

fiveZeroHashes :: Text -> [(Text, ByteString)]
fiveZeroHashes cipher =
  let padded = fmap ((cipher <>) . Text.pack . show) ([0..] :: [Int])
      hash' :: Text -> ByteString
      hash' =  B16.encode . BS.pack . BA.unpack . hashWith MD5 . Text.encodeUtf8
      fiveZeroes = BS.isPrefixOf "00000"
  in Prelude.filter (fiveZeroes . snd) . fmap (id &&& hash') $ padded

extractPassword :: [ByteString] -> Text
extractPassword = mconcat . take 8 . fmap (Text.pack . take 1 . drop 5 . Text.unpack . Text.decodeUtf8)

partA :: Text
partA = extractPassword . fmap snd . fiveZeroHashes $ "cxdnnyjw"

passwords :: [ByteString] -> [String]
passwords =
  let extractChar :: ByteString -> Maybe (Char, Int)
      extractChar s =
        let chars = Text.unpack . Text.decodeUtf8 $ s
            char = chars !! 6
            position =
              let c = chars !! 5
              in if isHexDigit c -- no safe way of doing this extraction in one go afaik
                 then Just . digitToInt $ c
                 else Nothing
        in sequence (char, position)
      collect :: Map Int Char -> (Char, Int) -> Map Int Char
      collect acc (c, pos) = case pos `Map.lookup` acc of
        Nothing -> Map.insert pos c acc
        Just _ -> acc
      inBounds = ((\i -> 0 <= i && i < 8) . snd)
  in fmap Map.elems . scanl collect mempty . filter inBounds . mapMaybe extractChar

extractPassword' :: [ByteString] -> Text
extractPassword' =
  let firstFull = Text.pack . head . filter ((> 7) . length)
  in firstFull . passwords

partB :: Text
partB = extractPassword' . fmap snd . fiveZeroHashes $ "cxdnnyjw"
