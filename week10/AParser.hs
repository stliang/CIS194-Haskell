{- CIS 194 HW 10
   due Monday, 1 April
-}
module AParser where

import Control.Applicative

import Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser
  { runParser :: String -> Maybe (a, String)
  }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing -- fail on the empty input
    f (x:xs) -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
      | p x = Just (x, xs)
      | otherwise = Nothing -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}
-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns = Nothing
      | otherwise = Just (read ns, rest)
      where
        (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------
first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

-- rp = runParser
-- codomain of rp is Maybe (a, String) becomes the domain of
-- fmap (first f) where first f changes a to b
instance Functor Parser where
  fmap f (Parser rp) = Parser (fmap (first f) . rp)

-- p1 <*> p2 represents the parser which first runs p1 (which will
-- consume some input and produce a function), then passes the
-- remaining input to p2 (which consumes more input and produces
-- some value), then returns the result of applying the function to the
-- value. However, if either p1 or p2 fails then the whole thing should
-- also fail (put another way, p1 <*> p2 only succeeds if both p1 and
-- p2 succeed).
instance Applicative Parser where
  pure a = Parser f
    where
      f str = Just (a, str)
  p1 <*> p2 = Parser f
    where
      f str =
        case runParser p1 str of
          Nothing -> Nothing
          Just (fRes, strRes) -> first fRes <$> runParser p2 strRes

type Name = String

data Employee = Emp
  { name :: Name
  , phone :: String
  }

lowerString :: Parser String
lowerString = Parser f
  where
    f xs
      | null ns = Nothing
      | otherwise = Just (ns, rest)
      where
        (ns, rest) = span isLower xs

parseName :: Parser Name
parseName = (:) <$> (satisfy isUpper) <*> lowerString

-- Emp <$> parseName <*> parsePhone :: Parser Employee
parseEmp :: Parser Employee
parseEmp = Emp <$> parseName <*> parsePhone

intString :: Parser String
intString = Parser f
  where
    f xs
      | null ns = Nothing
      | otherwise = Just (ns, rest)
      where
        (ns, rest) = span isDigit xs

parsePhone :: Parser String
parsePhone =
  (\a _ c _ e -> a ++ c ++ e) <$> intString <*> char '-' <*> intString <*> char '-' <*> intString

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = const () <$> abParser

intPair :: Parser [Integer]
intPair = (\a _ b -> [a, b]) <$> posInt <*> char ' ' <*> posInt

{-
class Applicative f =>
      Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
-}
instance Alternative Parser where
  empty = Parser (const Nothing)
  p1 <|> p2 = Parser f
    where
      f str = runParser p1 str <|> runParser p2 str

intOrUppercase :: Parser ()
intOrUppercase = const () <$> posInt <|> const () <$> satisfy isUpper
