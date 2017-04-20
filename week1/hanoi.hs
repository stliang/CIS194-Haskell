{-# OPTIONS_GHC -Wall #-}

type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n from to store
  | n == 1 = [(from, to)]
  | otherwise =
    (hanoi (n - 1) from store to) ++ (hanoi 1 from to store) ++ (hanoi (n - 1) store to from)
