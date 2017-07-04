{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.List

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n
  | n >= 2 = fib (n - 1) + fib (n - 2)
  | otherwise = 0

-- https://wiki.haskell.org/The_Fibonacci_sequence
fib2 :: Integer -> Integer
fib2 n = go n (0, 1)
  where
    go !n (!a, !b)
      | n == 0 = a
      | otherwise = go (n - 1) (b, a + b)

fibs1 :: [Integer]
fibs1 = fmap fib2 [0 ..]

-- Exercise 3 -----------------------------------------
data Stream a =
  Cons a
       (Stream a)

instance Show a =>
         Show (Stream a) where
  show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s) ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

-- Exercise 4 -----------------------------------------
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

-- Exercise 5 -----------------------------------------
nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) zs = Cons x (interleaveStreams zs xs)

ruler :: Stream Integer
ruler = startRuler 0

-- Eric D.Burgess - http://oeis.org/A001511
startRuler :: Integer -> Stream Integer
startRuler y = interleaveStreams (streamRepeat y) (startRuler (y + 1))

-- Exercise 6 -----------------------------------------
x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger n = Cons n (streamRepeat 0)
  negate (Cons y ys) = Cons (-y) (negate ys)
  (+) (Cons y ys) (Cons z zs) = Cons (y + z) (ys + zs)
  (*) (Cons y ys) s@(Cons z zs) = Cons (y * z) (streamMap (* y) zs + (ys * s))

instance Fractional (Stream Integer) where
  (/) (Cons y ys) (Cons z zs) = q
    where
      q = Cons (y `div` z) (streamMap (`div` z) (ys - q * zs))

fibs10 :: Stream Integer
fibs10 = x / (1 - x - x * x)

-- Exercise 7 -----------------------------------------
data Matrix =
  Matrix Integer
         Integer
         Integer
         Integer
  deriving (Show)

instance Num Matrix where
  (*) (Matrix a11 a12 a21 a22) (Matrix b11 b12 b21 b22) =
    (Matrix
       (a11 * b11 + a12 * b21)
       (a11 * b12 + a12 * b22)
       (a21 * b11 + a22 * b21)
       (a21 * b12 + a22 * b22))

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 1 = 1
fib4 n = getA11 (f ^ (n - 1))
  where
    f = Matrix 1 1 1 0

getA11 :: Matrix -> Integer
getA11 (Matrix a11 _ _ _) = a11
