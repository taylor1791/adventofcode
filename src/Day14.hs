{-# LANGUAGE FlexibleContexts, Rank2Types #-}
module Day14 where

import Data.List (maximumBy)
import Control.Monad (forM_)
import qualified Data.Map.Strict as M
import qualified Control.Monad.State.Lazy as S

import qualified Text.Parsec as P
import qualified Text.Parsec.Char as PC

type Parsec s u m b = P.Stream s m Char => P.ParsecT s u m b

type Rate = Int
type Period = Int
type Time = Int
type Distance = Int

bestDeer :: Int -> [(String, [(Rate, Time)])] -> [(String, Int)]
bestDeer time rates = winner
  where
    deer = map fst rates
    map0 = M.fromList $ zip deer $ repeat 0
    (winner, _) = S.runState (constructRun time) (map (fmap cycle) rates, map0, map0)

constructRun :: (S.MonadState ([(String, [(Rate, Period)])], M.Map String Int, M.Map String Int) m) => Int -> m [(String, Int)]
constructRun time = do
  forM_ [1..time] $ \_ -> do
    (rates, distances, totals) <- S.get -- :: ((String, rates), M.Map String Distance, M.Map String Int
    let deerRateAdvances = map (fmap advanceClock) rates
    let (rates', advances) = unzip $ map (\(x, (r, d)) -> ((x, r), (x, d))) deerRateAdvances
    let distances' = foldr (\(n, v) d -> advanceValue v n d) distances advances
    let leaders = map fst $ lead distances'
    let totals' = foldr (\x acc -> advanceWinner x acc) totals leaders
    S.put (rates', distances', totals')
  (_, _, points) <- S.get -- :: ((String, rates), M.Map String Distance, M.Map String Int
  return $ lead points

lead :: M.Map String Int -> [(String, Int)]
lead = M.foldrWithKey lead' []

lead' :: String -> Int -> [(String, Int)] -> [(String, Int)]
lead' k2 a2 [] = [(k2, a2)]
lead' k2 a2 xxs@((_, a1):_)
  | a1 == a2 = (k2, a2):xxs
  | a2 > a1  = [(k2, a2)]
  | a2 < a1  = xxs

advanceClock :: [(Rate, Period)] -> ([(Rate, Period)], Distance)
advanceClock ((_, 0):xs) = advanceClock xs
advanceClock ((x, y):xs) = ((x, y - 1):xs, x)

advanceValue :: Int -> String -> M.Map String Int -> M.Map String Int
advanceValue x = M.adjust (x+)

advanceWinner :: String -> M.Map String Int -> M.Map String Int
advanceWinner = advanceValue 1


-- Part 1

leadingDeer :: Time -> [(String, [(Rate, Period)])] -> (String, Distance)
leadingDeer time = maximumBy (\(_, x) (_, y) -> compare x y) . map (fmap (distance time))

distance :: Time -> [(Rate, Period)]  -> Distance
distance 0 _ = 0
distance time xxs@((rate, period):xs) =
  iterations * (distancePerIteration xxs) +
  periodTime * rate +
  distance (time - iterationsTime - periodTime) xs
  where
    totalTime = foldr (\x acc -> acc + snd x) 0 xxs
    iterations = time `div` totalTime
    iterationsTime = iterations * timePerIteration xxs
    periodTime = min (time - iterationsTime) period

distancePerIteration :: [(Rate, Period)] -> Distance
distancePerIteration = sum . uncurry (zipWith (*)) . unzip

timePerIteration :: [(Rate, Period)] -> Time
timePerIteration = sum . map snd

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

parse :: String -> Either P.ParseError (String, [(Distance, Rate)])
parse = P.parse parseDistance "(string)"

parseDistance :: Parsec s u m (String, [(Distance, Rate)])
parseDistance = do
  name <- parseIdentifier
  PC.string " can fly "
  rate <- parseInt
  PC.string " km/s for "
  duration <- parseInt
  PC.string " seconds, but then must rest for "
  restTime <- parseInt
  PC.string " seconds."
  return (name, [(rate, duration), (0, restTime)])

parseInt :: Parsec s u m Int
parseInt = read <$> P.many1 P.digit

parseIdentifier :: Parsec s u m String
parseIdentifier = P.many PC.letter

