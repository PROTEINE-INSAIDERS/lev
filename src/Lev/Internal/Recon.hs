{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

-- | Unwrapped reader + continuation monad transformer.
module Lev.Internal.Recon
  ( ReconT (..),
    evalReconT,
    mapReconT,
    module Control.Monad,
    module Control.Monad.Reader,
    module Control.Monad.Trans,
  )
where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans

-- | Reader + continuation (codensity?) monad transformer
newtype ReconT e m a = ReconT {runReconT :: forall r. e -> (a -> m r) -> m r} --TODO: add failure via Either?

{-# INLINE pureReconT #-}
pureReconT :: a -> ReconT e m a
pureReconT a = ReconT $ const ($ a)

{-# INLINE bindReconT #-}
bindReconT :: ReconT e m a -> (a -> ReconT e m b) -> ReconT e m b
bindReconT (ReconT f) g = ReconT $ \e k -> f e (\a -> runReconT (g a) e k)

{-# INLINE evalReconT #-}
evalReconT :: (Monad m) => ReconT e m a -> e -> m a
evalReconT m e = runReconT m e return

{-# INLINE mapReconT #-}
mapReconT :: (Monad m, Monad n) => (m a -> n b) -> ReconT e m a -> ReconT e n b
mapReconT f m = ReconT $ \e k -> f (evalReconT m e) >>= k

instance Functor (ReconT e m) where
  {-# INLINE fmap #-}
  fmap f = flip bindReconT (pureReconT . f)

instance Applicative (ReconT e m) where
  {-# INLINE pure #-}
  pure = pureReconT
  {-# INLINE (<*>) #-}
  f <*> a = bindReconT f $ flip fmap a

instance Monad (ReconT e m) where
  {-# INLINE (>>=) #-}
  (>>=) = bindReconT

instance (MonadFail m) => MonadFail (ReconT e m) where
  {-# INLINE fail #-}
  fail = lift . fail

instance MonadIO m => MonadIO (ReconT e m) where
  {-# INLINE liftIO #-}
  liftIO = lift . liftIO

instance MonadReader e (ReconT e m) where
  {-# INLINE ask #-}
  ask = ReconT $ flip id
  {-# INLINE local #-}
  local f m = ReconT $ runReconT m . f

instance MonadTrans (ReconT e) where
  {-# INLINE lift #-}
  lift m = ReconT $ const (m >>=)
