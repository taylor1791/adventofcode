module Day3 where

import Control.Monad (foldM)

import qualified Data.Map as M
import qualified Control.Monad.State as S

type Position = (Int, Int)
type HouseVisit a = S.State (M.Map Position Bool) a

next :: Position -> Char -> Position
next (x, y) '^' = (x, y + 1)
next (x, y) '>' = (x + 1, y)
next (x, y) 'v' = (x, y - 1)
next (x, y) '<' = (x - 1, y)
next _ _        = error "Day3.next: unknown direction"

deliver :: Position -> Char -> HouseVisit Position
deliver p m = S.get >>= S.put . M.insert p' True >> return p'
  where
    p' = next p m

deliverRoute :: String -> HouseVisit Position
deliverRoute = foldM deliver (0, 0)

runDelivery :: String -> (Position, M.Map Position Bool)
runDelivery r = S.runState (deliverRoute r) $ M.fromList [((0, 0), True)]

numHouses :: String -> Int
numHouses = M.size . snd . runDelivery

everyN :: Int -> String -> [String]
everyN n [] = take n $ repeat []
everyN n xs = zipWith (:) heads $ everyN n rest
  where
    (heads, rest) = splitAt n xs

runDeliveries :: Int -> String -> [(Position, M.Map Position Bool)]
runDeliveries n r
  | length r `mod` n /= 0 = error "Path length must be divisible by the number of deliveries"
  | otherwise             = map runDelivery $ everyN n r

numHousesN :: Int -> String -> Int
numHousesN n = M.size . M.unions . map snd . runDeliveries n
