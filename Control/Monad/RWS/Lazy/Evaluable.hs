{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Control.Monad.RWS.Lazy.Evaluable
( EvaluableT(..)
) where

import Prelude hiding (foldr)

import Control.Monad.RWS.Lazy (RWST(..))
import Data.Foldable (Foldable(foldr))
import Data.Functor.Identity (Identity(runIdentity))
import Data.Monoid (Monoid(mempty, mappend))

class (Monoid w) => EvaluableT r w s a where
  evaluateT :: (Monad m) => r -> s -> m (a, s, w)
  evaluateT r s =
    return (evaluate r s)
  
  evaluateTFold :: (Monad m, Foldable t, Monoid a) => t r -> s -> m (a, s, w)
  evaluateTFold =
    runRWST (foldRWST (RWST evaluateT))

  evaluateTFoldMap :: (Monad m, Foldable t, Monoid b) => (a -> b) -> t r -> s -> m (b, s, w)
  evaluateTFoldMap f =
    runRWST (foldMapRWST f (RWST evaluateT))

  evaluateTFoldr :: (Monad m, Foldable t) => (a -> b -> b) -> b -> t r -> s -> m (b, s, w)
  evaluateTFoldr f z =
    runRWST (foldrRWST f z (RWST evaluateT))
  
  evaluate :: r -> s -> (a, s, w)
  evaluate r s =
    runIdentity (evaluateT r s)
  
  evaluateFold :: (Foldable t, Monoid a) => t r -> s -> (a, s, w)
  evaluateFold tr s =
    runIdentity (evaluateTFold tr s)
  
  evaluateFoldMap :: (Foldable t, Monoid b) => (a -> b) -> t r -> s -> (b, s, w)
  evaluateFoldMap f tr s =
    runIdentity (evaluateTFoldMap f tr s)
  
  evaluateFoldr :: (Foldable t) => (a -> b -> b) -> b -> t r -> s -> (b, s, w)
  evaluateFoldr f z tr s =
    runIdentity (evaluateTFoldr f z tr s)

instance (Monoid w) => EvaluableT a w s a where
  evaluateT a s =
    return (a, s, mempty)

instance (Monoid w) => EvaluableT (s -> a) w s a where
  evaluateT f s =
    return (f s, s, mempty)

instance (Monoid w) => EvaluableT (s -> (a, s)) w s a where
  evaluateT f s =
    let (a, s') = f s in return (a, s', mempty)

foldRWST :: (Monad m, Monoid w, Foldable t, Monoid a) => RWST r w s m a -> RWST (t r) w s m a
foldRWST =
  foldMapRWST id

foldMapRWST :: (Monad m, Monoid w, Foldable t, Monoid b) => (a -> b) -> RWST r w s m a -> RWST (t r) w s m b
foldMapRWST f =
  foldrRWST (mappend . f) mempty

foldrRWST :: (Monad m, Monoid w, Foldable t) => (a -> b -> b) -> b -> RWST r w s m a -> RWST (t r) w s m b
foldrRWST f z (RWST run) =
  RWST $ foldr go (\s -> return (z, s, mempty))
    where
      go r acc =
        \s1 -> do
          (a1, s2, w1) <- run r s1
          (a2, s3, w2) <- acc s2
          return (a1 `f` a2, s3, w1 `mappend` w2)
