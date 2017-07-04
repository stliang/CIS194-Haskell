data BigRecord = BR
  { getName :: Name
  , getSSN :: String
  , getSalary :: Integer
  , getPhone :: String
  , getLicensePlate :: String
  , getNumSickDays :: Int
  }

type Name = String

data Employee = Employee
  { name :: Name
  , phone :: String
  } deriving (Show)

r = BR "Brent" "XXX-XX-XXX4" 600000000 "555-1234" "JGX-55T3" 2

getEmp :: BigRecord -> Employee
getEmp = Employee <$> getName <*> getPhone

ex01 = getEmp r

check :: Int -> Maybe Int
check n
  | n < 10 = Just n
  | otherwise = Nothing

halve :: Int -> Maybe Int
halve n
  | even n = Just $ n `div` 2
  | otherwise = Nothing

addOneOrTwo :: Int -> [Int]
addOneOrTwo x = [x+1, x+2]
