{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lev.Fixed (Serialize(..), decode, encode, makeSerialize, sizeOf) where

import Lev.Internal.Fixed.Core
import Lev.Internal.Fixed.ByteString
import Lev.Internal.Fixed.TH

mkPrimitiveSerializeInsances

-- instance Serialize m n Word8 where
--    type SerializeConstraint m o CBool = (PrimMonad m, KnownNat o)
