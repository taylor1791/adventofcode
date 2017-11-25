{-# LANGUAGE Rank2Types, FlexibleContexts #-}

module Day7 where

import qualified Data.Word as W
import qualified Data.Bits as B

import qualified Data.Map as M

import qualified Text.Parsec as P
import qualified Text.Parsec.Char as C

type Parsec s u m b = P.Stream s m Char => P.ParsecT s u m b

data CML = CML {gate :: Gate, target :: Source} deriving (Show, Eq)

data Identifier = Identifier {name :: String} deriving (Eq)

data Source = Source {identifier :: Identifier} | Signal {strength :: W.Word16}
  deriving (Show, Eq)

data Gate =
            Id Source
          | Not Source
          | And Source Source
          | Or Source Source
          | LShift Source Source
          | RShift Source Source
          deriving (Show, Eq)

parseCML :: Parsec s u m CML
parseCML = CML <$> parseLeft <*> parseIdentifier

parseLeft :: Parsec s u m Gate
parseLeft = parseGate >>= \g -> C.spaces >> consumeArrow >> return g

parseIdentifier :: Parsec s u m Source
parseIdentifier = P.many1 C.letter >>= \x -> C.spaces >> pure (Source (Identifier x))

parseGate :: Parsec s u m Gate
parseGate = foldr1 (\x acc -> acc P.<|> P.try x) $ map P.try 
  [
    parseId, parseNot, parseAnd, parseOr, parseLShift, parseRShift
  ]

consumeArrow :: Parsec s u m ()
consumeArrow = C.string "->" >> C.spaces

parseId :: Parsec s u m Gate
parseId = Id <$> parseSource

parseNot :: Parsec s u m Gate
parseNot = C.string "NOT" >> C.spaces >> Not <$> parseSource

parseAnd :: Parsec s u m Gate
parseAnd = parseInfix (P.string "AND") And

parseOr :: Parsec s u m Gate
parseOr = parseInfix (P.string "OR") Or

parseLShift :: Parsec s u m Gate
parseLShift = parseInfix (P.string "LSHIFT") LShift

parseRShift :: Parsec s u m Gate
parseRShift = parseInfix (P.string "RSHIFT") RShift

parseSource :: Parsec s u m Source
parseSource = P.try parseIdentifier P.<|> P.try parseSignal

parseSignal :: Parsec s u m Source
parseSignal = P.many1 P.digit >>= \x -> C.spaces >> pure (Signal $ read x)

parseInfix :: Parsec s u m String -> (Source -> Source -> Gate) -> Parsec s u m Gate
parseInfix s f = f <$> parseSource >>= \f' -> s >> C.space >> pure f' <*> parseSource

parse :: String -> CML
parse x = case P.parse parseCML "(source)" x of
  Left e -> error $ show e
  Right e -> e

instance Ord Identifier where
  compare x y = compare (name x) $ name y

instance Show Identifier where
  show = show . name

createSource :: [CML] -> M.Map Identifier W.Word16
createSource xs = result
  where
    result = foldr acc M.empty xs
    acc (CML g (Source i)) m = M.insert i (evalGate g result) m

evalGate :: Gate -> M.Map Identifier W.Word16 -> W.Word16
evalGate (Id x) m = evalSource x m
evalGate (Not x) m = B.complement $ evalSource x m
evalGate (And x y) m = (evalSource x m) B..&. (evalSource y m)
evalGate (Or x y) m = (evalSource x m) B..|. (evalSource y m)
evalGate (LShift x y) m = (evalSource x m) `B.shiftL` fromIntegral (evalSource y m)
evalGate (RShift x y) m = (evalSource x m) `B.shiftR` fromIntegral (evalSource y m)

evalSource :: Source -> M.Map Identifier W.Word16  -> W.Word16
evalSource (Source x) m = evalIdentifier x m
evalSource (Signal x) _ = x

evalIdentifier :: Identifier -> M.Map Identifier W.Word16 -> W.Word16
evalIdentifier x = M.findWithDefault (error $ "Cannot find " ++ show x) x

-- This in an un-momoized version. Impossible long to solve. 
{-
createMap :: [CML] -> M.Map Identifier Gate
createMap = foldr (\(CML g (Source i)) m -> M.insert i g m) (M.fromList [])

evalGate :: M.Map Identifier Gate -> Gate -> W.Word16
evalGate m (Id x) = evalSource m x
evalGate m (Not x) = B.complement $ evalSource m x
evalGate m (And x y) = (evalSource m x) B..&. (evalSource m y)
evalGate m (Or x y) = (evalSource m x) B..|. (evalSource m y)
evalGate m (LShift x y) = (evalSource m x) `B.shiftL` fromIntegral (evalSource m y)
evalGate m (RShift x y) = (evalSource m x) `B.shiftR` fromIntegral (evalSource m y)

evalSource :: M.Map Identifier Gate -> Source -> W.Word16
evalSource m (Source x) = evalIdentifier m x
evalSource _ (Signal x) = x

evalIdentifier :: M.Map Identifier Gate -> Identifier -> W.Word16
evalIdentifier m x = evalGate m $ M.findWithDefault (error $ "Cannot find " ++ show x) x m
-}
