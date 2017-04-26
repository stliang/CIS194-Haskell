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

-- Refactor the functions below with Bernd Schwarzenbacher's solutions:
--
{-| The 'localMaxima' function calculates the local maxima of three consecutive
 numbers from a list of type 'Integer'.  It then accumulates the results in a
 list.
 It takes a list of type 'Integer' and outputs a list of type 'Integer'.
-}
localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:zs)
  | x < y && y > z = y : localMaxima (y : z : zs)
  | otherwise = localMaxima (y : z : zs)
localMaxima _ = []

{-| The 'histogram' function transforms a list of type 'Integer' to
 a histogram of the number of occurrence of each number.
 It does this by framing the histogram by a list generator [m + 1,m .. 1],
 and within this frame, it fills it with contain generated by its support
 function 'line'.

 For example:
 histogram [4,3] =
 "          \n   **     \n==========\n0123456789\n"
-}
histogram :: [Integer] -> String
histogram xs = unlines (map (line c) [m + 1,m .. 1]) ++ "==========\n0123456789\n"
  where
    c = count xs
    m = maximum c

{-| The 'line' function uses a list element generator to generate a row
 of a histogram represented in type 'String'.
 It takes a list of 'Int' representing the number of occurrence per number
 and the row number in type 'Int'.

 For example:
 line [4,4,1,1,0,0,0,0,0,0] 1 =
 "****      "
-}
line :: [Int] -> Int -> String
line xs n =
  [ if i >= n
    then '*'
    else ' '
  | i <- xs
  ]

{-| The 'count' function generates a list of occurrence counts of a given
 list of 'Integers'. It does this with filter which creates groups of Intergers
 occurrence in the order of 0 to 9, followed by the length function sums the number
 of occurrence per number position along the x-axis.

 For example:
 map (\n -> filter (== n) [0,3,2,1,0,1,0,1,1,0]) [0 .. 9] =
 [[0,0,0,0],[1,1,1,1],[2],[3],[],[],[],[],[],[]]  

 map (\n -> length $ filter (== n) [0,3,2,1,0,1,0,1,1,0]) [0 .. 9] = 
 [4,4,1,1,0,0,0,0,0,0]
-}
count :: [Integer] -> [Int]
count xs = map (\n -> length $ filter (== n) xs) [0 .. 9]
