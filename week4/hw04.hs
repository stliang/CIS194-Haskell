{-# OPTIONS_GHC -Wall #-}

module Hw04 where

import Data.List

{-|
 Exercise 1: Wholemeal programming
 The following solutions are from Bernd Schwarzenbacher
-}
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

{-|
 This none point free style code is much harder to understand
 when comparing to the point free version in fun2'.  I thought
 fun2 generates a tree in the process, but turned out it is a
 list.
-}
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

{-|
 This point free version clearly says it will generate a list
 with the lambda in iterate, then combined with the effects of
 the rest of the functions.
-}
fun2' :: Integer -> Integer
fun2' =
  sum .
  filter even .
  takeWhile (/= 1) .
  iterate
    (\n ->
       if even n
         then n `div` 2
         else 3 * n + 1)

{-|
 Exercise 2: Folding with trees
 This solution is from Andrew Bennett using AVL Tree
 http://en.wikipedia.org/wiki/AVL_tree
-}
data Tree a
  = Leaf
  | Node Integer
         (Tree a)
         a
         (Tree a)
  deriving (Show, Eq)

foldTree
  :: Ord a
  => [a] -> Tree a
foldTree = foldr avlInsert Leaf

balanceFactor :: Tree a -> Integer
balanceFactor = avlBalanceFactor

height :: Tree a -> Integer
height = avlHeight

avlInsert
  :: Ord a
  => a -> Tree a -> Tree a
avlInsert k Leaf = Node 0 Leaf k Leaf
avlInsert k (Node _ l k1 r) =
  if k <= k1
    then let l' = avlInsert k l
         in avlBalance (Node (avlMaxHeight l' r) l' k1 r)
    else let r' = avlInsert k r
         in avlBalance (Node (avlMaxHeight l r') l k1 r')

avlHeight :: Tree a -> Integer
avlHeight Leaf = 0
avlHeight (Node h _ _ _) = h

avlMaxHeight :: Tree a -> Tree a -> Integer
avlMaxHeight a b = 1 + max (avlHeight a) (avlHeight b)

avlBalance
  :: (Ord a)
  => Tree a -> Tree a
avlBalance Leaf = Leaf
avlBalance t@(Node _ l k r)
  | abs (avlBalanceFactor t) < 2 = t
  | avlHeight l < avlHeight r =
    case r of
      Leaf -> error "cannot rotate a leaf"
      (Node _ l1 k1 r1) ->
        let child = (Node (avlMaxHeight l l1) l k l1)
        in (Node (avlMaxHeight child r1) child k1 r1)
  | otherwise =
    case l of
      Leaf -> error "cannot rotate a leaf"
      (Node _ l1 k1 r1) ->
        let child = (Node (avlMaxHeight r1 r) r1 k r)
        in (Node (avlMaxHeight l1 child) l1 k1 child)

avlBalanceFactor :: Tree a -> Integer
avlBalanceFactor Leaf = 0
avlBalanceFactor (Node _ l _ r) = avlHeight l - avlHeight r

{-|
  Exercise 3: More folds!
  Solution from Bernd Schwarzenbacher
  xor2 models oxr logic and xor is then implemented with foldr
-}
xor :: [Bool] -> Bool
xor = foldr xor2 False

xor2 :: Bool -> Bool -> Bool
xor2 a b = (a || b) && not (a && b)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> f x : y) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f x = foldr (flip f) x . reverse

{-|
 Exercise 4: Finding primes
 Solution from Bernd Schwarzenbacher
-}
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+ 1) . (* 2)) $ [1 .. n] \\ sieve
  where
    sieve =
      map (\(i, j) -> i + j + 2 * i * j) . filter (\(i, j) -> i + j + 2 * i * j <= n) $
      cartProd [1 .. n] [1 .. n]

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]
