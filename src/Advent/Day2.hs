{-# LANGUAGE LambdaCase #-}
module Advent.Day2 where

import Control.Arrow (first, second)
import Control.Applicative ((<|>))
import Control.Lens
import Data.Foldable
import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Text.Lens
import Text.Megaparsec
import Text.Megaparsec.Text

type Key = (Int, Int)
data Keypad = Keypad {
    _keys :: Map Key Text
  , _start :: Key
  } deriving (Show)
data Movement = U | L | D | R
  deriving (Show, Eq)

parseMovements :: Text -> [[Movement]]
parseMovements input =
  case runParser parser "" $ input of
    Left err -> fail . parseErrorPretty $ err
    Right x -> x
  where
    parser :: Parser [[Movement]]
    parser = row `sepBy` newline
    row = some (l <|> r <|> u <|> d)
    u = U <$ char 'U'
    d = D <$ char 'D'
    l = L <$ char 'L'
    r = R <$ char 'R'

squareKeypad :: Keypad
squareKeypad = Keypad {
    _keys = Map.fromList $ ikeys ^.. (itraversed<.>itraversed) . to show . packed . withIndex
  , _start = (2,2) -- the 5 key
  }
  where
    ikeys = (fmap Map.fromList) . Map.fromList . zip [1..] . fmap (zip [1..]) $ keys
    keys :: [[Int]]
    keys = [[1,2,3]
           ,[4,5,6]
           ,[7,8,9]]

move :: Keypad -> Key -> Movement -> Key
move Keypad{..} key movement =
  let nextKey = reifyMovement movement $ key
  in if nextKey `Map.member` _keys
     then nextKey
     else key
  where
    reifyMovement = \case
      U -> first  pred
      R -> second succ
      D -> first  succ
      L -> second pred

decodePIN :: Keypad -> Text -> Text
decodePIN pad@Keypad{..} =
  let lineToChar = foldl' $ move pad
      chars = tail . scanl lineToChar _start
  in foldMap (_keys !) . chars . parseMovements

diamondKeypad :: Keypad
diamondKeypad = Keypad {
    _keys = Map.fromList $ ikeys ^.. (itraversed<.>itraversed) . filtered (/= ' ' ) . to (\x -> [x]) . packed . withIndex
  , _start = (3,1) -- the 5 key
  }
  where
    ikeys = (fmap Map.fromList) . Map.fromList . zip [1..] . fmap (zip [1..]) $ keys
    keys =  ["  1  "
            ," 234 "
            ,"56789"
            ," ABC "
            ,"  D  "]
