{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}


module Lev.Internal.Fixed.Test where

import Control.Monad.Primitive (PrimMonad)
import Data.Functor.Indexed (IxApplicative, (*>>), IxPointed (ireturn), (<<$>>), (<<*>>))
import Data.Word (Word8, Word16, Word32)
import GHC.Generics (Generic)
import GHC.TypeLits (KnownNat, type (+))
import Language.Haskell.TH (runQ, instanceD, Type)
import Language.Haskell.TH.Lib (stringE)
import Language.Haskell.TH.Syntax (reify)
import Lev.Fixed
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder, byteStringHex, toLazyByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Int
import Data.Data (Typeable, Proxy (Proxy))

data Test0 a = Test0 Double Int Word8 deriving (Show, Generic)

data Test1 = Test1 Word8 (Test0 Int) deriving (Show, Generic)

data Test0G a = Test0G  Word8 Test2G Test2G deriving (Show, Generic)

data Test1G = Test1G Word8 (Test0G Int) deriving (Show, Generic)

data Test2G = Test2G Int Int deriving (Show, Generic)

instance Serialize m n (Test0G a)

instance Serialize m n Test1G

instance Serialize m n Test2G

makeSerialize ''Test0

makeSerialize ''Test1

data Test  = Test deriving Show

data SmallSum = SS1 | SS2 Test1G    deriving (Generic, Show, Typeable, Serialize m n)

-- test :: IO SmallSum
-- test = encode SS1 >>= decode
