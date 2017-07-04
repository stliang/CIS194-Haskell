module Party where

import Data.Monoid
import Data.Tree
import Employee

main :: IO ()
main = readFile "company.txt" >>= putStrLn . computeOutput

computeOutput :: String -> String
computeOutput = formatGL . maxFun . read

formatGL :: GuestList -> String
formatGL (GL lst fun) = "Total fun: " ++ show fun ++ "\n" ++ unlines employees
  where
    employees = map (\(Emp {empName = name}) -> name) lst

glCons :: Employee -> GuestList -> GuestList
glCons emp@(Emp {empFun = ef}) (GL el fun) = GL (el ++ [emp]) (ef + fun)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL el1 fun1) (GL el2 fun2) = GL (el1 ++ el2) (fun1 + fun2)
  mconcat [] = GL [] 0
  mconcat gl@(x:xs) = foldr (\x y -> mappend x y) (GL [] 0) gl

moreFun :: GuestList -> GuestList -> GuestList
moreFun a b =
  if a > b
    then a
    else b

treeFold :: (a -> [b] -> b) -> b -> Tree a -> b
treeFold f init (Node {rootLabel = rl, subForest = sf}) = f rl (map (treeFold f init) sf)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss bestLists = (maximumS withBossL, maximumS withoutBossL)
  where
    withoutBossL = map fst bestLists
    withoutSubBoss = map snd bestLists
    withBossL = map (glCons boss) withoutSubBoss

maximumS
  :: (Monoid a, Ord a)
  => [a] -> a
maximumS [] = mempty
maximumS lst = maximum lst

maxFun :: Tree Employee -> GuestList
maxFun tree = uncurry moreFun tu
  where
    tu = treeFold nextLevel (mempty, mempty) tree

--Test dat
emp1 = Emp {empFun = 44, empName = "Steve"}

emp2 = Emp {empFun = 43, empName = "Abby"}

emp3 = Emp {empFun = 24, empName = "Jack"}

emp4 = Emp {empFun = 23, empName = "Jill"}

emp5 = Emp {empFun = 44, empName = "Pat"}

emp6 = Emp {empFun = 43, empName = "Donna"}

gl1 = GL [emp1, emp2] 87

gl2 = GL [emp3, emp4] 47

gl3 = GL [emp1, emp2] 87

gl4 = GL [emp5, emp6] 87

l1 = [gl1, gl2]
