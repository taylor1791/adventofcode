{-# LANGUAGE OverloadedStrings #-}
module Day12 where

import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
import qualified Data.Scientific as S
import Data.Aeson

aesonFold :: Monoid a => (a -> Value -> a) -> a -> Value -> a
aesonFold f x0 (Null) = f x0 Null
aesonFold f x0 (Bool x) = f x0 $ Bool x
aesonFold f x0 (Number x) = f x0 $ Number x
aesonFold f x0 (String x) = f x0 $ String x
aesonFold f x0 (Array x) = V.foldl f x0 x
aesonFold f x0 (Object x) = H.foldl' f x0 x

numAdd :: S.Scientific -> Value -> S.Scientific
numAdd x (Number y) = x `mappend` y
numAdd x (Array y) = V.foldl (\acc v -> aesonFold numAdd acc v) x y
numAdd x (Object y) = H.foldl' (\acc h -> aesonFold numAdd acc h) x y
numAdd x _ = x

filterAeson :: (Value -> Bool) -> Value -> Value
filterAeson f v@(Object x)
  | f v       = Null
  | otherwise = Object $ H.map (filterAeson f) x
filterAeson f (Array x) = Array $ V.map (filterAeson f) x
filterAeson _ x = x

isRed :: Value -> Bool
isRed (Object x) = H.foldl' (\acc y -> acc || y == "red") False x
isRed _ = False

sumNonRedNumbers :: Value -> S.Scientific
sumNonRedNumbers = sumNumbers . filterAeson isRed 

sumNumbers :: Value -> S.Scientific
sumNumbers = aesonFold numAdd mempty

instance Monoid S.Scientific where
  mempty = 0
  mappend = (+)
