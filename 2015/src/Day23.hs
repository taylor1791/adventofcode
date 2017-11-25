{-# LANGUAGE FlexibleContexts, Rank2Types #-}
module Day23 where

import qualified Data.Vector as V

import qualified Text.Parsec as P
import qualified Text.Parsec.Char as PC

type Parsec s u m b = P.Stream s m Char => P.ParsecT s u m b

data Reg = A | B deriving Show
data Instr = Hlf Reg | Tpl Reg | Inc Reg | Jmp Int | Jie Reg Int | Jio Reg Int deriving Show

data Machine = Machine {
               isp :: Int
             , a   :: Int
             , b   :: Int
             } deriving Show

init :: Machine
init = Machine 0 0 0

run :: V.Vector Instr -> Machine -> Machine
run p = go
  where
    l = length p
    go m
      | isp m >= l || isp m < 0 = m
      | otherwise               = go $ runInstr p m

runInstr :: V.Vector Instr -> Machine -> Machine
runInstr p m = eval (p V.! isp m) m

eval :: Instr -> Machine -> Machine
eval (Hlf A) m = m { isp = isp m + 1, a = a m `div` 2 }
eval (Hlf B) m = m { isp = isp m + 1, b = b m `div` 2 }
eval (Tpl A) m = m { isp = isp m + 1, a = a m * 3 }
eval (Tpl B) m = m { isp = isp m + 1, b = b m * 3 }
eval (Inc A) m = m { isp = isp m + 1, a = a m + 1 }
eval (Inc B) m = m { isp = isp m + 1, b = b m + 1 }
eval (Jmp x) m = m { isp = isp m + x }
eval (Jie A x) m
  | even (a m) = m { isp = isp m + x }
  | otherwise  = m { isp = isp m + 1 }
eval (Jie B x) m
  | even (b m) = m { isp = isp m + x }
  | otherwise  = m { isp = isp m + 1 }
eval (Jio A x) m
  | 1 == a m  = m { isp = isp m + x }
  | otherwise  = m { isp = isp m + 1 }
eval (Jio B x) m
  | odd (b m)  = m { isp = isp m + x }
  | otherwise  = m { isp = isp m + 1 }

--------------------------------------------------------------------------------

parse :: String -> Either P.ParseError (V.Vector Instr)
parse = fmap V.fromList . mapM (P.parse parseInstr "(string)") . lines

parseRegister :: Parsec s u m Reg
parseRegister = P.try (PC.char 'a' >> return A) P.<|> P.try (PC.char 'b' >> return B)

parseSign :: Parsec s u m (Int -> Int)
parseSign = P.try (PC.char '+' >> return (1*)) P.<|> P.try (PC.char '-' >> return (0-))

parseOffset :: Parsec s u m Int
parseOffset = parseSign <*> (read <$> P.many1 P.digit)

parseUnary :: (a -> Instr) -> String -> Parsec s u m a -> Parsec s u m Instr
parseUnary f s c = f <$> (P.string s >> c)

parseBinary :: (a -> b -> Instr) -> String -> Parsec s u m a -> Parsec s u m b -> Parsec s u m Instr
parseBinary f s c1 c2 = f <$> (P.string s >> c1) <*> (P.string ", " >> c2)

parseInstr :: Parsec s u m Instr
parseInstr = foldl1 (\acc x -> acc P.<|> P.try x) [
    parseUnary Hlf "hlf " parseRegister
  , parseUnary Tpl "tpl " parseRegister
  , parseUnary Inc "inc " parseRegister
  , parseUnary Jmp "jmp " parseOffset
  , parseBinary Jie "jie " parseRegister parseOffset
  , parseBinary Jio "jio " parseRegister parseOffset
  ]



