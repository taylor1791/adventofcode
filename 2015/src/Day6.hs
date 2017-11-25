{-# LANGUAGE FlexibleContexts, Rank2Types #-}
module Day6 where

import Control.Monad

import Data.Array.ST
import Data.Array.Unboxed

import qualified Text.Parsec as P
import Text.Parsec.Char (char, string)

type Position = (Int, Int)
type Parsec s u m b = P.Stream s m Char => P.ParsecT s u m b

data Instruction =
    On {start :: Position, end :: Position}
  | Toggle {start :: Position, end :: Position}
  | Off {start :: Position, end :: Position}
  deriving (
    Eq
  , Show
  )

data Action =
    AOn {position :: Position}
  | AOff {position :: Position}
  | AToggle {position :: Position}
  deriving (
    Eq
  , Show
  )

generateActions :: Instruction -> [Action]
generateActions x@(On _ _) = map AOn $ generateRange (start x) (end x)
generateActions x@(Toggle _ _) = map AToggle $ generateRange (start x) (end x)
generateActions x@(Off _ _) = map AOff $ generateRange (start x) (end x)

generateRange :: Position -> Position -> [Position]
generateRange (x1, y1) (x2, y2) = [(x, y) | x <- [x1..x2], y <- [y1..y2]]

f :: Monoid a => Array (Int, Int) a
f = runSTArray $ do
  ls <- newArray ((0, 0), (999, 999)) mempty
  return ls

-- The original implementation uses unboxed arrays and it was much faster.
-- However, I cannot use typeclasses then. This is slow, but reusable. More
-- important in this case
-- lightLights :: Light a => [Instruction] -> UArray (Int, Int) a
-- lightLights insts = runSTUArray $ do

lightLights :: Light a => [Instruction] -> Array (Int, Int) a
lightLights insts = runSTArray $ do
  ls <- newArray ((0, 0), (999, 999)) zero
  forM_ insts $ \inst -> do
    forM_ (generateActions inst) $ \action -> do
      let pos = position action
      value <- readArray ls pos
      writeArray ls pos $ apply value action
  return ls

class Light a where
  zero :: a
  apply :: a -> Action -> a
  lights :: Array (Int, Int) a -> Int

instance Light Bool where
  zero = False
  lights = length . filter id . elems
  apply b (AToggle _) = not b
  apply _ (AOn _) = True
  apply _ (AOff _) = False

instance Light Int where
  zero = 0
  lights = sum . elems
  apply x (AToggle _) = x + 2
  apply x (AOn _) = x + 1
  apply x (AOff _) = max 0 $ x - 1

(<|>) :: Parsec s u m a -> Parsec s u m a -> Parsec s u m a
(<|>) = (P.<|>)

readInt :: Parsec s u m Int
readInt = read <$> P.many1 P.digit

readComma :: Parsec s u m Char
readComma = char ','

readPair :: Parsec s u m Position
readPair = do
  x <- readInt
  readComma
  y <- readInt
  return (x, y)

readThrough :: Parsec s u m String
readThrough = string " through "

readOn :: Parsec s u m (Position -> Position -> Instruction)
readOn = do
  string "turn on "
  return On

readOff :: Parsec s u m (Position -> Position -> Instruction)
readOff = do
  string "turn off "
  return Off

readToggle :: Parsec s u m (Position -> Position -> Instruction)
readToggle = do
  string "toggle "
  return Toggle

readInstruction :: Parsec s u m (Position -> Position -> Instruction)
readInstruction = P.try readOn <|> P.try readOff <|> P.try readToggle

readFullInstruction :: Parsec s u m Instruction
readFullInstruction = do
  ins <- readInstruction
  s <- readPair
  readThrough
  e <- readPair
  return $ ins s e

parse :: String -> Instruction
parse = right . P.parse readFullInstruction "(source)"

right :: Either a b -> b
right x = case x of
  Left _ -> error $ "failed to parse"
  Right e -> e

