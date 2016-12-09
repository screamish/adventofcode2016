module Advent.Day1 where

import Control.Applicative ((<|>))
import Data.List (foldl')
import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Text
import Text.Megaparsec.Lexer (integer)

type Position = (Integer, Integer)
data Direction = N | E | S | W deriving (Show, Eq, Bounded, Enum)
data Turn = R | L deriving (Show, Eq)
type Distance = Integer
type Movement = (Turn, Distance)

turn :: Direction -> Turn -> Direction
turn d L = if d == minBound then maxBound else pred d
turn d R = if d == maxBound then minBound else succ d

move :: Position -> Direction -> Distance -> Position
move (x,y) N d = (x,   y+d)
move (x,y) E d = (x+d, y)
move (x,y) S d = (x,   y-d)
move (x,y) W d = (x-d, y)

origin :: (Position, Direction)
origin = ((0,0), N)

bunny :: [Movement] -> Position
bunny =
  let next :: (Position, Direction) -> Movement -> (Position, Direction)
      next (p, dir) (t, dist) =
        let dir' = turn dir t
        in (move p dir' dist, dir')
  in fst . foldl' next origin

parseMovements :: Text -> [Movement]
parseMovements input =
  case runParser parser "" $ input of
    Left err -> fail . parseErrorPretty $ err
    Right x -> x
  where
    parser :: Parser [Movement]
    parser = (l <|> r) `sepBy` (between space space $ char ',')
    l = (L,) <$> (char 'L' *> integer)
    r = (R,) <$> (char 'R' *> integer)
