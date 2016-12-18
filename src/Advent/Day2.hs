{-# LANGUAGE LambdaCase #-}
module Advent.Day2 where

import Control.Arrow (first, second, (>>>))
import Control.Applicative ((<|>))
import Control.Lens
import Data.Function ((&))
import Data.Foldable
import Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Sequence (ViewR(..))
import qualified Data.Sequence as Seq
import           Data.Sequence.NonEmpty (NonEmptySeq(..))
import qualified Data.Sequence.NonEmpty as NESeq
import Data.Text (Text)
import Data.Text.Lens
import Text.Megaparsec
import Text.Megaparsec.Text
import Text.Megaparsec.Lexer (integer)

type Key = (Int, Int)
type Keypad = Map Key Text
data Movement = U | L | D | R
  deriving (Show, Eq)

parse :: Text -> [[Movement]]
parse input =
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

keypad :: Keypad
keypad = Map.fromList $ ikeys ^.. (itraversed<.>itraversed) . to show . packed . withIndex
  where
    ikeys = (fmap Map.fromList) . Map.fromList . zip [1..] . fmap (zip [1..]) $ keys
    keys :: [[Int]]
    keys = [[1,2,3]
           ,[4,5,6]
           ,[7,8,9]]

move :: Key -> Movement -> Key
move key movement =
  let nextKey = reifyMovement movement $ key
  in if nextKey `Map.member` keypad
     then nextKey
     else key
  where
    reifyMovement = \case
      U -> first  pred
      R -> second succ
      D -> first  succ
      L -> second pred

five :: Key
five = (2,2)

decodePIN :: Text -> Text
decodePIN input =
  let movements = parse input
  in foldMap (move )
