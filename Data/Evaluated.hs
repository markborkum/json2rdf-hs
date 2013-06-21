{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Data.Evaluated
( Evaluated(..)
) where

import Prelude hiding (foldr)

import Control.Monad.Trans.RWS.Lazy (RWST(..))
import Data.Foldable (Foldable(foldr))
import Data.Monoid (Monoid(mempty, mappend))

class (Monoid w) => Evaluated r w s a where
  evaluate :: (Monad m) => r -> s -> m (a, s, w)
  
  evaluateFold :: (Monad m, Foldable t, Monoid a) => t r -> s -> m (a, s, w)
  evaluateFold =
    runRWST (foldRWST (RWST evaluate))

  evaluateFoldMap :: (Monad m, Foldable t, Monoid b) => (a -> b) -> t r -> s -> m (b, s, w)
  evaluateFoldMap f =
    runRWST (foldMapRWST f (RWST evaluate))

  evaluateFoldr :: (Monad m, Foldable t) => (a -> b -> b) -> b -> t r -> s -> m (b, s, w)
  evaluateFoldr f z =
    runRWST (foldrRWST f z (RWST evaluate))

instance (Monoid w) => Evaluated a w s a where
  evaluate a s =
    return (a, s, mempty)

instance (Monoid w) => Evaluated (s -> a) w s a where
  evaluate f s =
    return (f s, s, mempty)

instance (Monoid w) => Evaluated (s -> (a, s)) w s a where
  evaluate f s =
    let (a, s') = f s in return (a, s', mempty)

foldRWST :: (Monad m, Monoid w, Foldable t, Monoid a) => RWST r w s m a -> RWST (t r) w s m a
foldRWST =
  foldMapRWST id

foldMapRWST :: (Monad m, Monoid w, Foldable t, Monoid b) => (a -> b) -> RWST r w s m a -> RWST (t r) w s m b
foldMapRWST f =
  foldrRWST (mappend . f) mempty

foldrRWST :: (Monad m, Monoid w, Foldable t) => (a -> b -> b) -> b -> RWST r w s m a -> RWST (t r) w s m b
foldrRWST f z (RWST g) =
  RWST $ foldr go (\s -> return (z, s, mempty))
    where
      go r acc =
        \s1 -> do
          (a1, s2, w1) <- g r s1
          (a2, s3, w2) <- acc s2
          return (a1 `f` a2, s3, w1 `mappend` w2)
