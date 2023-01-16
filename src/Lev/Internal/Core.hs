{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Lev.Internal.Core where

import Data.Data (TypeRep)
import Data.Functor.Contravariant (Contravariant (contramap))
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Data.Word (Word8)
import GHC.Generics (C1, Generic (Rep, from), K1 (K1, unK1), M1 (M1, unM1), U1, type (:*:) ((:*:)), type (:+:) (L1, R1))
import GHC.TypeLits (CmpNat, ErrorMessage (Text), KnownNat, Nat, TypeError, natVal, type (+), type (<=?))
import GHC.Types (Constraint)



data Fixed

data Variable

class Lev l a | a -> l where
  type SizeOf l a :: Nat
  type SizeOf l a = GSizeOf l (Rep a)
  size :: a -> Int
  default size :: (Generic a, GLev l (Rep a)) => a -> Int
  size = gSize @l . from

class GLev l f where
  type GSizeOf l f :: Nat
  gSize :: f a -> Int

instance KnownNat (GSizeOf Fixed f) => GLev Fixed f where
  type GSizeOf Fixed f = GFixedSizeOf f
  gSize = const $ fromIntegral $ natVal $ Proxy @(GFixedSizeOf f)

type family GFixedSizeOf f where
  GFixedSizeOf (M1 _ _ f) = GFixedSizeOf f
  GFixedSizeOf U1 = 0
  GFixedSizeOf (K1 _ a) = SizeOf Fixed a
  GFixedSizeOf (f :*: g) = GFixedSizeOf f + GFixedSizeOf g
  GFixedSizeOf (f :+: g) = SizeOfTagResult (SumArity (f :+: g) <=? 256) + GFixedSumSizeOf (f :+: g)

type family SumArity a where
  SumArity (C1 c a) = 1
  SumArity (x :+: y) = SumArity x + SumArity y

type MoreThan256ConstructorsMessage = 'Text "Generic deriving of Lev instances can only be used on datatypes with fewer than 256 constructors."

type family SizeOfTagResult b where
  SizeOfTagResult 'True = SizeOf Fixed Word8
  SizeOfTagResult 'False = TypeError MoreThan256ConstructorsMessage

type family OrdCond o lt eq gt where
  OrdCond 'LT lt eq gt = lt
  OrdCond 'EQ lt eq gt = eq
  OrdCond 'GT lt eq gt = gt

type Max (m :: Nat) (n :: Nat) = OrdCond (CmpNat m n) n n m :: Nat

type family GFixedSumSizeOf f where
  GFixedSumSizeOf (f :+: g) = Max (GFixedSumSizeOf f) (GFixedSumSizeOf g)
  GFixedSumSizeOf (M1 ti c f) = GSizeOf Fixed f

instance (GVariableSize f) => GLev Variable f where
  type GSizeOf Variable f = TypeError ('Text "GSizeOf: Variable.")
  gSize = gVariableSize

class GVariableSize f where
  gVariableSize :: f a -> Int

instance GVariableSize f => GVariableSize (M1 ti c f) where
  gVariableSize = gVariableSize . unM1

instance GVariableSize U1 where
  gVariableSize = const 0 -- actually fixed, but never mind

instance Lev l a => GVariableSize (K1 c a) where
  gVariableSize = size . unK1

instance (GVariableSize f, GVariableSize g) => GVariableSize (f :*: g) where
  gVariableSize (a :*: b) = gVariableSize a + gVariableSize b

type FitsInByte n = FitsInByteResult (n <=? 255)

type family FitsInByteResult (b :: Bool) :: Constraint where
  FitsInByteResult 'True = ()
  FitsInByteResult 'False = TypeError MoreThan256ConstructorsMessage

instance (FitsInByte (SumArity (f :+: g)), GVariableSumSize (f :+: g)) => GVariableSize (f :+: g) where
  gVariableSize a = gVariableSumSize a + size (undefined :: Word8)

class GVariableSumSize f where
  gVariableSumSize :: f a -> Int

instance GVariableSize f => GVariableSumSize (M1 ti c f) where
  gVariableSumSize = gVariableSize . unM1

instance (GVariableSumSize f, GVariableSumSize g) => GVariableSumSize (f :+: g) where
  gVariableSumSize (L1 a) = gVariableSumSize a
  gVariableSumSize (R1 b) = gVariableSumSize b

#include "MachDeps.h"

instance Lev Fixed Int where
  type SizeOf Fixed Int = SIZEOF_HSINT
  {-# INLINE size #-}
  size = const $ fromIntegral $ natVal $ Proxy @(SizeOf Fixed Int)

instance Lev Fixed Word8 where
  type SizeOf Fixed Word8 = SIZEOF_WORD8
  {-# INLINE size #-}
  size = const $ fromIntegral $ natVal $ Proxy @(SizeOf Fixed Word8)

data Size (fixed :: Bool) a where 
  FixedSize :: Int -> Size 'True a 
  VariableSize ::  (a -> Int) -> Size 'False a 

class Sized l a where
  type IsFixed l :: Bool
  sized :: Size (IsFixed l) a

instance (Lev Fixed a) => Sized Fixed a where
  type IsFixed Fixed = 'True
  sized = FixedSize $ size (undefined :: a)

instance (Lev Variable a) => Sized Variable a where
  type IsFixed Variable = 'False
  sized = VariableSize $ size @Variable @a

instance (Sized l a, Lev l a) => Lev Variable [a] where
  type SizeOf Variable [a] = TypeError ('Text "SizeOf [a]: Variable")
  {-# INLINE size #-}
  size xs = case sized @l @a of
    FixedSize sz -> length xs * sz
    VariableSize f -> sum $ f <$> xs



---------------------------------------


data Test = Test0 Int | Test1 | Test2 deriving (Show, Eq, Generic, Lev Fixed)

test0 = size $ Test0 0

data TestFixedData = TestFixedData1 Int | TestFixedData2 Int Int deriving (Show, Eq, Generic, Lev Fixed)

data TestVariableData = TestVariableData0 | TestVariableData1 Int TestFixedData deriving (Show, Eq, Generic, Lev Variable)

testData1 = TestVariableData1 10 $ TestFixedData1 20

test1 = size testData1

testData2 = [testData1, testData1]

test2 = size testData2

testData3 = [testData2, testData2]

test3 = size testData3

test4 = size (TestVariableData0)
