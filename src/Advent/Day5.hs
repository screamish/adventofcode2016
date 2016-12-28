{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase #-}
module Advent.Day5 where

import Control.Arrow ((&&&), first)
import qualified Data.ByteArray as BA
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Monoid
import qualified Data.ByteString.Base16 as B16
import Data.Char (ord)
import Data.List (group, sort, sortBy)
import Data.Ord (comparing, Down(..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Crypto.Hash.Algorithms (MD5(..))
import           Crypto.Hash (Digest, hashWith)

fiveZeroHashes :: Text -> [(Text, ByteString)]
fiveZeroHashes cipher =
  let padded = fmap ((cipher <>) . Text.pack . show) [0..]
      hash' :: Text -> ByteString
      hash' =  B16.encode . BS.pack . BA.unpack . hashWith MD5 . Text.encodeUtf8
      fiveZeroes = BS.isPrefixOf "00000"
  in Prelude.filter (fiveZeroes . snd) . fmap (id &&& hash') $ padded

extractPassword :: [ByteString] -> Text
extractPassword = mconcat . take 8 . fmap (Text.pack . take 1 . drop 5 . Text.unpack . Text.decodeUtf8)

partA :: Text
partA = extractPassword . fmap snd . fiveZeroHashes $ "cxdnnyjw"
