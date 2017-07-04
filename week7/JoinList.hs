{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module JoinList where

import Data.Monoid

import Buffer
import Editor
import Scrabble
import Sized

data JoinList m a
  = Empty
  | Single m
           a
  | Append m
           (JoinList m a)
           (JoinList m a)
  deriving (Eq, Show)

instance Buffer (JoinList (Score, Size) String) where
  toString = unlines . jlToList
  fromString = foldl (\jl str -> jl +++ scoreLine' str) Empty . lines
    where
      scoreLine' str = Single (scoreString str, 1) str
  line = indexJ
  replaceLine n str jl = takeJ n jl +++ fromString str +++ dropJ (n + 1) jl
  numLines = getSize . snd . tag
  value = getScore . fst . tag

main = runEditor editor (fromString "test" :: (JoinList (Score, Size) String))

(+++)
  :: Monoid m
  => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (tag a <> tag b) a b

tag
  :: Monoid m
  => JoinList m a -> m
tag (Single m _) = m
tag (Append m _ _) = m
tag _ = mempty

indexJ
  :: (Sized b, Monoid b)
  => Int -> JoinList b a -> Maybe a
indexJ index (Single _ a)
  | index == 0 = Just a
  | otherwise = Nothing
indexJ index (Append m l1 l2)
  | index < 0 || index > size0 = Nothing
  | index < size1 = indexJ index l1
  | otherwise = indexJ (index - size1) l2
  where
    size0 = getSize . size $ m
    size1 = getSize . size . tag $ l1
indexJ _ _ = Nothing

dropJ
  :: (Sized b, Monoid b)
  => Int -> JoinList b a -> JoinList b a
dropJ n l1@(Single _ _)
  | n <= 0 = l1
dropJ n l@(Append m l1 l2)
  | n >= size0 = Empty
  | n >= size1 = dropJ (n - size1) l2
  | n > 0 = dropJ n l1 +++ l2
  | otherwise = l
  where
    size0 = getSize . size $ m
    size1 = getSize . size . tag $ l1
dropJ _ _ = Empty

takeJ
  :: (Sized b, Monoid b)
  => Int -> JoinList b a -> JoinList b a
takeJ n l1@(Single _ _)
  | n > 0 = l1
takeJ n l@(Append m l1 l2)
  | n >= size0 = l
  | n >= size1 = l1 +++ takeJ (n - size1) l2
  | n > 0 = takeJ n l1
  where
    size0 = getSize . size $ m
    size1 = getSize . size . tag $l1
takeJ _ _ = Empty

scoreLine :: String -> JoinList Score String
scoreLine str = Single (scoreString str) str

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

a =
  Append
    (Size 3)
    (Append (Size 2) (Single (Size 1) "hi") (Single (Size 1) "bye"))
    (Single (Size 1) "tschau")

b = Single (Size 1) "blub"

c = Append (Size 2) (Single (Size 1) "hi") (Single (Size 1) "bye")

d = Append (Size 2) (Single (Size 1) "see") (Single (Size 1) "you")

e = Append (Size 2) (Single (Size 1) "what's") (Single (Size 1) "up?")

f = Append (Size 2) (Single (Size 1) "airrow") (Single (Size 1) "plane")

z = (c +++ d) +++ (e +++ f)

z' =
  Append
    (Size 6)
    (Append (Size 2) (Single (Size 1) "hi") (Single (Size 1) "bye"))
    (Append
       (Size 4)
       (Append (Size 2) (Single (Size 1) "see") (Single (Size 1) "you"))
       (Append (Size 2) (Single (Size 1) "what's") (Single (Size 1) "up?")))

z'' =
  Append
    (Size 8)
    (Append
       (Size 4)
       (Append (Size 2) (Single (Size 1) "hi") (Single (Size 1) "bye"))
       (Append (Size 2) (Single (Size 1) "see") (Single (Size 1) "you")))
    (Append
       (Size 4)
       (Append (Size 2) (Single (Size 1) "what's") (Single (Size 1) "up?"))
       (Append (Size 2) (Single (Size 1) "airrow") (Single (Size 1) "plane")))
