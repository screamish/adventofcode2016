module Advent.Day1 where

import Control.Arrow (first, second, (>>>))
import Control.Applicative ((<|>))
import Data.Function ((&))
import Data.Foldable
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import           Data.Sequence (ViewR(..))
import qualified Data.Sequence as Seq
import           Data.Sequence.NonEmpty (NonEmptySeq(..))
import qualified Data.Sequence.NonEmpty as NESeq
import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Text
import Text.Megaparsec.Lexer (integer)

type Position = (Integer, Integer)
data Direction = N | E | S | W deriving (Show, Eq, Bounded, Enum)
type Bunny = (Position, Direction)
data Turn = R | L deriving (Show, Eq)
type Distance = Integer
type Movement = (Turn, Distance)

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

turn :: Turn -> Direction -> Direction
turn L d = if d == minBound then maxBound else pred d
turn R d = if d == maxBound then minBound else succ d

move :: Position -> Direction -> Distance -> Position
move (x,y) N d = (x,   y+d)
move (x,y) E d = (x+d, y)
move (x,y) S d = (x,   y-d)
move (x,y) W d = (x-d, y)

origin :: Bunny
origin = ((0,0), N)

bunny :: [Movement] -> Position
bunny =
  let next :: (Position, Direction) -> Movement -> (Position, Direction)
      next (p, dir) (t, dist) =
        let dir' = turn t dir
        in (move p dir' dist, dir')
  in fst . foldl' next origin

move1 :: Bunny -> Bunny
move1 (pos, dir) = (go dir pos, dir)
  where
    go N = second succ
    go E = first  succ
    go S = second pred
    go W = first  pred

turnBunny :: Turn -> Bunny -> Bunny
turnBunny t = second $ turn t

type Path = NonEmptySeq Bunny

getLast :: NonEmptySeq a -> a
getLast NonEmptySeq{..} = lastAft _aft
  where
    lastAft = Seq.viewr >>> \case
      EmptyR -> _fore
      _ :> l -> l

growPath :: Movement -> Path -> Path
growPath (t, dist) p =
  let turned = turnBunny t . getLast $ p
      nextSegment :: Seq.Seq Bunny
      nextSegment = Seq.drop 1 $ Seq.scanl (&) turned $ Seq.replicate (fromIntegral dist) move1
  in p `NESeq.appendSeq` nextSegment

bunnyPath :: [Movement] -> Path
bunnyPath = foldl' (flip growPath) (NESeq.singleton origin)

firstDuplicate :: (Ord a, Foldable f) => f a -> Maybe a
firstDuplicate = snd . foldl' (flip go) (mempty, Nothing)
  where
    go :: Ord a =>  a -> (Set.Set a, Maybe a) -> (Set.Set a, Maybe a)
    go _ acc@(_, Just _) = acc
    go a (seen, Nothing) =
      let found = if a `Set.member` seen
                  then Just a
                  else Nothing
      in (a `Set.insert` seen, found)

-- | bunny' stops on the first position visited twice
bunny' :: [Movement] -> Maybe Position
bunny' = firstDuplicate . fmap fst . bunnyPath
