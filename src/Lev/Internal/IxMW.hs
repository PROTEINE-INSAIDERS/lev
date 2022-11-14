{-# LANGUAGE PolyKinds #-}

-- | A wrapper to convert a monad to an indexed monad.
module Lev.Internal.IxMW where

import Control.Monad.Indexed
  ( IxApplicative (..),
    IxFunctor (..),
    IxMonad (..),
    IxPointed (..),
  )

newtype IxMW m i j a = IxMW {unIxMW :: m a}

instance (Functor m) => IxFunctor (IxMW m) where
  {-# INLINE imap #-}
  imap f = IxMW . fmap f . unIxMW

instance (Applicative m) => IxPointed (IxMW m) where
  {-# INLINE ireturn #-}
  ireturn = IxMW . pure

instance (Applicative m) => IxApplicative (IxMW m) where
  {-# INLINE iap #-}
  iap (IxMW f) (IxMW a) = IxMW $ f <*> a

instance (Monad m) => IxMonad (IxMW m) where
  {-# INLINE ibind #-}
  ibind g (IxMW f) = IxMW $ f >>= unIxMW . g
