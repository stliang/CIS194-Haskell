{-# LANGUAGE MultiParamTypeClasses #-}

module MonadT where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans

-- runMaybeT is a field of MaybeT'
--
-- MaybeT' is isomorphic to runMaybeT
-- MaybeT' :: m (Maybe a) -> MaybeT' m a
-- runMaybeT :: MaybeT' m a -> m (Maybe a)
newtype MaybeT' m a = MaybeT'
  { runMaybeT :: m (Maybe a)
  }

-- Now I have a newtype which suggests wrapping of
-- Maybe a in m is equivalent to MaybeT' m a.  This
-- is an identity construction.  What to do next?
-- I want to make MaybeT' a proper Monad Transformer
-- in which the character of Maybe can be part of a
-- larger monad.  At the larger monad level of
-- abstraction, different inner monads can be treated
-- the same.  That is smooth composition without
-- the need of lambda bridging different monads.
-- Since MaybeT' type exists now, can I create a MaybeT'?
-- Yes, I can just make a MaybeT' like this:
-- *MonadT> m = MaybeT' [(Just 1)]
-- *MonadT> :t m
-- m :: Num a => MaybeT' [] a
--
-- But it is not part of the Monad Transformer type class.
-- So, any instances of MaybeT' as of now can not participate
-- at the higher level abstration MonadTrans offers.
-- Wanted to have the smoothness of ManadTrans offers should
-- be the reason this newtype has the shape of:
-- MaybeT' m a.  To see this, first see the kind signature of
-- MonadTrans:
-- *MonadT Control.Monad.Trans> :info MonadTrans 
-- class MonadTrans (t :: (* -> *) -> * -> *) where
--   lift :: Monad m => m a -> t m a
--     {-# MINIMAL lift #-}
--          -- Defined in ‘Control.Monad.Trans.Class’
--
-- This say in order to be an instance of MonadTrans,
-- I need to implement lift.  Once successfully
-- implimented lift, my MaybeT' can plug itself into
-- the MonadTrans machinery.  So, what shape MaybeT'
-- has to have in this case?  Can I deduce its shape
-- from MonadTrans?  Well, at the class level, MonadTrans
-- say it has a shape of t :: (* -> *) -> * -> *.  Does this
-- mean MaybeT' also needs to have this shape?  If so, why?
-- In order to be in the class, I just need to implement lift
-- and nothing else.  So, I should just do that and the shape
-- of MaybeT' should come out of that requirement.  But before
-- I begain, I should understand the shapes of all components
-- of lift.  The method lift has the following components:
--
-- m   * -> *  
--
-- This m is a type constructor as it takes a concrete type
-- and in return gives me another concrete type.  This type
-- constructor is constrainted to be a Monad.
--
-- a   *
--
-- This a is a concrete type
--
-- t   (* -> *) -> * -> *
--
-- This t is another type constructor but it takes two inputs,
-- the first being another constructor that takes only one input,
-- follow by a concret type.  Then it gives back another concrete
-- type.  What is interesting here is t m a looks like an onion.
-- My question is if I have a shape that looks like e d c b a,
-- what shape would I get from Haskell?  My guess is the following
-- shape:
--
-- a   *
-- b   * -> *
-- c   (* -> *) -> * -> *
-- d   ((* -> *) -> * -> *) -> (* -> *) -> * -> *
--     ((* -> *) -> * -> *) -> (* -> *) -> * -> *
-- e   (((* -> *) -> * -> *) -> (* -> *) -> * -> *) -> ((* -> *) -> * -> *)
--     -> (* -> *) -> * -> *
--
-- That is by the position of the argument, haskell deduces the shape,
-- and the way it does it, is I think by the consumption of argument
-- where the last one is a concrete type.  So, b consumes a to produce
-- a concrete type, c sonsumes b and a to produce a concrete type and
-- so on.
-- To prove this, I will create a type class:
class Onion1 c where
  lift1 :: b a -> c b a

class Onion2 c d where
  lift2 :: c b a -> b a -> d c b a

class Onion3 c d e where
  lift3 :: d c b a -> c b a -> b a -> d c b a -> e d c b a

class Onion4 c d e where
  lift4 :: b a -> e d c b a

class Onion5 e where
  lift5 :: d c b a -> c b a -> b a -> d c b a -> e d c b a

{-
 It turned out the onion idea only applies to the input types:

*MonadT Control.Monad.Trans> :info Onion1
class Onion1 (c :: (* -> *) -> * -> *) where
      lift1 :: b a -> c b a
  {-# MINIMAL lift1 #-}

*MonadT Control.Monad.Trans> :info Onion2
class Onion2 (c :: (* -> *) -> * -> *)
             (d :: ((* -> *) -> * -> *) -> (* -> *) -> * -> *) where
                   lift2 :: c b a -> b a -> d c b a
  {-# MINIMAL lift2 #-}

*MonadT Control.Monad.Trans> :info Onion3
class Onion3 (c :: (* -> *) -> * -> *)
             (d :: ((* -> *) -> * -> *) -> (* -> *) -> * -> *)
             (e :: (((* -> *) -> * -> *) -> (* -> *) -> * -> *)
                   -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *) where
                         lift3 :: d c b a -> c b a -> b a -> d c b a -> e d c b a
  {-# MINIMAL lift3 #-}

*MonadT Control.Monad.Trans> :info Onion4
class Onion4 c d (e :: * -> * -> (* -> *) -> * -> *) where
      lift4 :: b a -> e d c b a
  {-# MINIMAL lift4 #-}


Notice Onion4's e is not the same as the e in Onion3 even though the output type
are both e d c b a.
-}
-- In a single parameter type class, the parameter mentioned in the type
-- class models the class of types.  In the case of class MonadTrans t :: (* -> *) -> * -> *
-- or t m a, m is a monad and a is a concrete type.  Types that fit
-- this shape can be a member of this class.  At this point, I am guess I can
-- use the newtype expression to make a t m a but just rename t to what ever
-- monadic behavior I want to be composable with other monads.
-- That is where the newtype MaybyT' m a comes from.  But what about the m (Maybe a)?
-- The type constructor MaybyT' represents the t in MonadTrans, now I need the
-- inject my Maybe behavior into m a.  Since m takes a concret type a and a can be anything
-- that generate a concrete type, I can just say a is to be produced by Maybe a.
-- So, that is where the m (Maybe a) comes from.  So, again, m (Maybe a) and MaybeT' m a
-- equivalent.
--
-- Since MaybeT' and t of MonadTrans have the same kind signature, MaybeT' can be
-- a member of MonadTrans:
--
-- *MonadT> :t MaybeT'
-- MaybeT' :: m (Maybe a) -> MaybeT' m a
-- *MonadT> :k MaybeT'
-- MaybeT' :: (* -> *) -> * -> *
--
-- Notice that there is no memtion that m is a monad, but the fact that the input
-- parameter m takes a concrete type produced by (Maybe a) says that m is a
-- type constructor with the kind of (* -> *), (Maybe a) then is just a *.
-- After taking in these two parameters, MaybeT' data constructor is supposed to 
-- produce another * that has the type MaybeT' m a.  Note that m in MaybeT' m a
-- does not say it is a monad or type constructor, but because m (Maybe a) and 
-- MaybeT' m a are isomorphic by newtypes single data constructor and single
-- field constraint, we can be sure m is a type constructor and a is anything that
-- resulting in a concrete type so I can just treate it as a concrete type.
--
-- So, finally I can make MaybeT' and instance of MonadTrans.  Since MaybeT'
-- matches MonadTrans' t exactly, I should be able to just replace t with 
-- MaybeT'.
instance MonadTrans MaybeT' where
  lift mx = MaybeT' (mx >>= return . Just)

-- The mx is constraint to be a monad by MonadTrans type class definition
-- This allows me to have access to the >>= funtion which takes the value
-- out of mx monad and gives it to the Just function as input.  Just will
-- produce "Maybe a", but MaybeT' expects to get m (Maybe a) as input,
-- so Just is composed with return function which wrapping the (Maybe a) in 
-- m.  Note that >>= allows us to compose recursively within the mx monad.
-- That is all the functions bind with >>= never left the mx monad.  This
-- gives it the monadic character of function dependency and preserve type
-- definition at the outter most box.  I think this >>= is call an endofunctor.
--
-- So, now what?  How do I use this MaybeT' in a meaningful way?  Adding the
-- layer of MonadTransformer promises a new leverl of smoothness.  I am
-- supposed to be able to combind two monads into one and process that monad
-- without any special treatment of the two.  How to test this out?
--
-- Before that happens, I should be able to use MaybeT' alone.  From the
-- lift, I should be able to wrap a value into any monad and lift it
-- into the MonadTrans context.
r = MaybeT' [(Just 1)]

-- There is not much I can do with MaybeT' by itself as MaybeT' is just a
-- newtype made of existing types.  What ever can be done with MaybeT' m a is
-- provided by m (Maybe a).  The only thing that is new here is
-- runMaybeT.  The real benefit is MaybeT' combined with another monad.  A
-- typical example from the net with combining with the IO monad:
-- See https://en.wikibooks.org/wiki/Haskell/Monad_transformers
--
-- Let's say I want to read someting that may or may not be the input I am
-- looking for.  How would one read in an input in the first place?  I think
-- it is the getLine method.
-- So, here I should be able to us a do block to get input, valid it, and send
-- output right?
isValid :: String -> Bool
isValid s = length s >= 8

{-
do
  s <- lift getLine
  return s
-}
-- It turns out that making my MaybeT' an instance of MonadTrans is not the only
-- requirement to smoothness.  MaybeT' itself has to be an instance of Monad.
-- To do that, I need to following what ghci says:
-- class Applicative m => Monad (m :: * -> *) where
--   (>>=) :: m a -> (a -> m b) -> m b
--   (>>) :: m a -> m b -> m b
--   return :: a -> m a
--   fail :: String -> m a
--   {-# MINIMAL (>>=) #-}
--
-- I read that the fail method is put there by mistake, so, I will not
-- implement fail and >> should be defined by >>= by default.
-- So, my MaybeT' has the signature of (* -> *) -> * -> * while monad has the
-- the signature of * -> *.  This means I would need to partially apply
-- m to MaybeT'.  The m then is upto the consumer the funtion MaybeT'
-- to provide.
--
-- First, I would like to implement return :: a -> m a
-- As far as monad is concern, the return . Just should be transparent 
-- because MaybeT' is now an instance of Monad.  So MaybeT' on the surface
-- is an monad, the return . Just is the partial application that I 
-- just mentioned.  But then is composition the same as partial application?
--
-- Second, I now need to impliment this  (>>=) :: m a -> (a -> m b) -> m b
-- What is ma? So, m is now (MaybeT' m) while a is still a.  I
-- need to substitute m for MaybeT' m.  I should get
-- (>>=) :: MaybeT' m a -> (a -> MaybeT' m b) -> MaybeT' m b
-- MaybeT' method take m (Maybe a) and returns MaybeT' m a.  So
-- I need to make a (m (Maybe a)) and pass it to MaybeT'.
instance Monad m =>
         Monad (MaybeT' m) where
  return a = MaybeT' $ return (Just a)
  x >>= f =
    MaybeT' $ do
      maybe_value <- runMaybeT x -- resulting m (Maybe a)
      case maybe_value of
        Just a -> runMaybeT (f a)
        Nothing -> return Nothing

-- Why the following don't work?
--  (>>=) mx f = do
--    x <- mx
--    f x
-- The reason is ma is actually MaybeT' m a
-- the do assigns ma to x not a.  But MaybeT' don't have >>= yet, so
-- it can not assign in the first place.  The only option is to
-- construct a MaybeT' and change the concrete type a to b as I have
-- done above.
--
-- It appears that MaybtT' has to be an instance of Applicative and
-- Functor type classes because Monad is an Applicative and Applicative
-- is a Functor.  One can look at 
-- https://hackage.haskell.org/package/transformers-0.5.4.0/docs/src/Control.Monad.Trans.Maybe.html
-- for the actual implementation of MaybeT type.
--
-- This is not what I originally thought MaybeT would work out.  I
-- thought MaybeT' only need to be an instance of MonadTrans and that is
-- that.  Now it turned out I have to implement instances of the whole
-- monad stack. (Monad, Applicative, and Functor).
-- In a way, this make sense because the promise is MaybeT is a monad
-- which can combined with other monad such as IO, List, etc.  So
-- MaybeT should have all the behaviors that a Monad should have.
--
-- Instead of implementing Applicative and Functor instances myself,
-- I will just copy from transformers package:
mapMaybeT :: (m (Maybe a) -> n (Maybe b)) -> MaybeT' m a -> MaybeT' n b
mapMaybeT f = MaybeT' . f . runMaybeT

instance (Functor m) =>
         Functor (MaybeT' m) where
  fmap f = mapMaybeT (fmap (fmap f))

instance (Functor m, Monad m) =>
         Applicative (MaybeT' m) where
  pure = lift . return
  mf <*> mx =
    MaybeT' $ do
      mb_f <- runMaybeT mf
      case mb_f of
        Nothing -> return Nothing
        Just f -> do
          mb_x <- runMaybeT mx
          case mb_x of
            Nothing -> return Nothing
            Just x -> return (Just (f x))

-- Now that MaybeT' is functional, I can test its smoothness:
-- Going back to my original intent to get a password from
-- user, validate it, and output an appropriate response.
-- On one hand, I am concerned about handling computations
-- and on the other hand, I am concerned with boxing and
-- unboxing.  The boxing stuff I mean context management as in
-- the monadic contexts.  I should be aboe to apply my validation
-- function to the IO function.  Let me try:
test :: IO [Char]
test = do
  b <- (fmap isValid getLine)
  case b of
    True -> return "Ha"
    False -> return "Ba"

-- Nothing wrong with the above code is there?  It does what I want
-- and it is easy to understand what is going on. This is all done
-- without the MaybeT'.  Where is the benefit?  OK, Let me pull
-- in an example of MaybeT from wikibook:
instance Monad m =>
         Alternative (MaybeT' m) where
  empty = MaybeT' $ return Nothing
  x <|> y =
    MaybeT' $ do
      maybe_value <- runMaybeT x
      case maybe_value of
        Nothing -> runMaybeT y
        Just _ -> return maybe_value

instance Monad m =>
         MonadPlus (MaybeT' m) where
  mzero = empty
  mplus = (<|>)

getPassphrase :: MaybeT' IO String
getPassphrase = do
  s <- lift getLine
  guard (isValid s)
  return s

askPassphrase :: MaybeT' IO ()
askPassphrase = do
  lift $ putStrLn "Insert your new passphrase:"
  value <- getPassphrase
  lift $ putStrLn "Storing in database ..."

-- Looks to me the benefit is in the guard and that the Maybe context is
-- handled by the transformer machinary.
--
-- Now test this
main :: IO (Maybe ())
main = runMaybeT askPassphrase
