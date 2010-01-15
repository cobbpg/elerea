{-|

This branch is an experimental version of Elerea that does not build
an actual graph of the dataflow network, just maintains a list of
actions to update signals.  Each signal consists of a mutable
variable, an aging action and a finalising action.  The variables can
only be accessed through a sampling action, and they are only referred
to in the corresponding aging and finalising action.  These actions
can be accessed through weak pointers that get invalidated when all
other references to the corresponding variable are lost.

This approach has both advantages and disadvantages.  On the plus
side, we don't have to create nodes for the applicative operations any
more, because they can be encoded in the sampling actions in an
efficient way.  Also, since we have a list of independent actions to
update the network, we can achieve nearly perfect parallelism, just
like with a raytracer: all we need is a clever way of assigning
actions to processing units.  The downside is that we have to
explicitly memoise the results of applicative operations in case they
are used more than once.

The modules below implement the basic idea in two variations:

* "FRP.Elerea.Experimental.Simple" provides discrete signals,
  i.e. streams;

* "FRP.Elerea.Experimental.Param" adds an extra parameter that's
  accessible to every node during the update, which can be used to
  provide a time step between samplings, or any other input necessary;

* "FRP.Elerea.Experimental.Delayed" adds automatic delays, which
  violates referential transparency in a limited way, but improves the
  usability of the API when this doesn't matter.

This module exports the delayed version along with a few utility
functions.

-}

module FRP.Elerea.Experimental
       ( module FRP.Elerea.Experimental.Delayed
       , (-->)
       , edge
       , (==@), (/=@), (<@), (<=@), (>=@), (>@)
       , (&&@), (||@)
       ) where

import Control.Applicative
import FRP.Elerea.Experimental.Delayed

infix  4 ==@, /=@, <@, <=@, >=@, >@
infixr 3 &&@
infixr 2 ||@
infix  2 -->

-- | The 'edge' transfer function takes a bool signal and emits
-- another bool signal that turns true only at the moment when there
-- is a rising edge on the input.
edge :: Signal p Bool -> SignalGen p (Signal p Bool)
edge b = delay True b >>= \db -> return $ (not <$> db) &&@ b

-- | The '-->' transfer function behaves as a latch on a 'Maybe'
-- input: it keeps its state when the input is 'Nothing', and replaces
-- it with the input otherwise.
(-->) :: a                        -- ^ Initial output
      -> Signal p (Maybe a)       -- ^ Maybe signal to latch on
      -> SignalGen p (Signal p a)
x0 --> s = transfer x0 store s
    where store _ Nothing  x = x
          store _ (Just x) _ = x

-- | Point-wise equality of two signals.
(==@) :: Eq a => Signal p a -> Signal p a -> Signal p Bool
(==@) = liftA2 (==)

-- | Point-wise inequality of two signals.
(/=@) :: Eq a => Signal p a -> Signal p a -> Signal p Bool
(/=@) = liftA2 (/=)

-- | Point-wise comparison of two signals.
(<@) :: Ord a => Signal p a -> Signal p a -> Signal p Bool
(<@) = liftA2 (<)

-- | Point-wise comparison of two signals.
(<=@) :: Ord a => Signal p a -> Signal p a -> Signal p Bool
(<=@) = liftA2 (<=)

-- | Point-wise comparison of two signals.
(>=@) :: Ord a => Signal p a -> Signal p a -> Signal p Bool
(>=@) = liftA2 (>=)

-- | Point-wise comparison of two signals.
(>@) :: Ord a => Signal p a -> Signal p a -> Signal p Bool
(>@) = liftA2 (>)

-- | Point-wise OR of two boolean signals.
(||@) :: Signal p Bool -> Signal p Bool -> Signal p Bool
s1 ||@ s2 = s1 >>= \b -> if b then return True else s2

-- | Point-wise AND of two boolean signals.
(&&@) :: Signal p Bool -> Signal p Bool -> Signal p Bool
s1 &&@ s2 = s1 >>= \b -> if b then s2 else return False
