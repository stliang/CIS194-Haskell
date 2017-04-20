{-# OPTIONS_GHC -Wall #-}

toDigits :: Integer -> [Integer]
toDigits x
  | x == 0 = []
  | x < 0 = []
  | otherwise = toDigits (x `div` 10) ++ [x `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x == 0 = []
  | x < 0 = []
  | otherwise = (x `mod` 10) : toDigitsRev (x `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = x : []
doubleEveryOther (x:(y:zs)) = x : y * 2 : doubleEveryOther zs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:[]) = x
sumDigits (x:(y:zs)) = (x + y) + sumDigits zs

toSingleDigit :: [Integer] -> [Integer]
toSingleDigit [] = []
toSingleDigit (x:[]) = toDigits x
toSingleDigit (x:zs) = toDigits (x) ++ toSingleDigit zs

validate :: Integer -> Bool
validate x = (sumDigits $ toSingleDigit $ doubleEveryOther $ toDigitsRev x) `mod` 10 == 0
