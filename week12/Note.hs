import Control.Applicative
import Control.Monad
import Control.Monad.Random
import Control.Monad.Trans.State

getRandom'
  :: Random a
  => State StdGen a
getRandom' = state random

threeInts :: Rand StdGen (Int, Int, Int)
threeInts = getRandom >>= \i1 -> getRandom >>= \i2 -> getRandom >>= \i3 -> return (i1, i2, i3)

someTypes :: State StdGen (Int, Float, Char)
someTypes = liftA3 (,,) getRandom' getRandom' getRandom'

allTypes :: State StdGen (Int, Float, Char, Integer, Double, Bool, Int)
allTypes =
  (,,,,,,) <$> getRandom' <*> getRandom' <*> getRandom' <*> getRandom' <*> getRandom' <*> getRandom' <*>
  getRandom'

test :: (Int, Float, Char, Integer, Double, Bool, Int)
test = evalState allTypes (mkStdGen 0)

-- data Fix f = Fix (f (Fix f))
-- data Free f r = Free (f (Free f r)) | Pure r
-- data List a   = Cons  a (List a  )  | Nil
--
-- t :: (* -> *) -> * -> *
-- t takes a type constructor and a concrete type then returns a concrete type
--
newtype MaybeT m a = MaybeT
  { runMaybeT :: m (Maybe a)
  }
