module Golf where

import Data.List (group, sort)
import Prelude hiding (head, tail, init, (!!))

{-| The 'every' function drops the nth -1 element of a list.
 It takes 'n' as the nth element of type 'Int' and 'xs' of type 'list'.
 This function is found online.
-}
every n xs =
  case drop (n - 1) xs of
    (y:ys) -> y : every n ys
    [] -> []

{-| The 'skips' function turns a list of elements into a list of list of elements.
 The first list in the output is the same as the input list. 
 The second list in the output contains every second element from the input list
 and the nth list in the output contains every nth element from the input list.
 It takes a list of any type.
-}
skips :: [a] -> [[a]]
skips [] = []
skips xs = map (\i -> every i xs) [1 .. (length xs)]

{-
-- Found slidingWindow online, works nicely on list
-- more general but only known to work on list
slidingWindow step width xs
  | len < width = []
  | otherwise = [(take width xs)] ++ slidingWindow step width (drop step xs)
  where
    len = length xs
-}
{-| The 'localMaxima' function takes a list of number, then generate a list
 of 3 number windows based upon the original list of numbers.  It calculates
 each windows' localMaxima if they exist.  It then folds the result list
 to clean up any empty list returning all the local maximas in a list.
 It takes a list of type 'Integer' and outputs a list of type 'Integer'.
-}
localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima as = foldl (\x y -> x ++ (f y)) [] (listWindow as)
  where
    f (x:y:z:[])
      | x < y && y > z = [y]
      | otherwise = []
    f _ = []
    listWindow as = map (\i -> take 3 (drop i as)) [0 .. (length as) - 3]

{-| The 'stack' function takes a list of list of 'Integer'
  that should be sorted and grouped. It regroups them based on
  the nth position of each element. This is to prepare
  the points of each row in a plot table in histogram.
  It takes a list of list of type 'Integer' and returns
  a list of list of type 'Integer'.
  This function should be called like so: stack $ group $ sort xs
-}
stack :: [[Integer]] -> [[Integer]]
stack [] = []
stack xs = [line] ++ stack remainder
  where
    remainder = filter (/= []) (map (\y -> drop 1 y) xs)
    line = foldl (\acc ys -> acc ++ (take 1 ys)) [] xs

{-| The 'stackToString' converts a list of list of type
  'Integer' into rowis of histogram points representation
  in list of list of type 'Char'.
-}
stackToString :: [[Integer]] -> [[Char]]
stackToString [] = []
stackToString (x:ys) =
  [ (map
       (\a ->
          if (elem a x)
            then '*'
            else ' ')
       [0 .. 9])
  ] ++
  stackToString ys

{-| The 'histogram' function converts a list of type 'Integer'
 to a histogram represented in String.  The return value can
 be outputed by putStr.
 It takes a list of type 'Integer'
-}
histogram :: [Integer] -> String
histogram [] = []
histogram as = foldr (++) "" (body ++ [xAxis])
  where
    body = map (\a -> a ++ "\n") (stackToString $ reverse $ stack $ group $ sort as)
    xAxis = "==========\n0123456789\n"
