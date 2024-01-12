{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}

module Lev.Internal.Core where

import Control.Monad.Indexed (IxApplicative, IxFunctor, IxMonad, IxPointed, (>>>=))
import Data.Data (TypeRep)
import Data.Functor.Contravariant (Contravariant (contramap))
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Data.Word (Word8)
import Foreign (Ptr)
import GHC.Generics (C1, Generic (Rep, from, to), K1 (K1, unK1), M1 (M1, unM1), U1, type (:*:) ((:*:)), type (:+:) (L1, R1))
import GHC.TypeLits (CmpNat, ErrorMessage (Text), KnownNat, Nat, TypeError, natVal, type (+), type (<=?))
import GHC.Types (Constraint)
import Lev.Internal.IxMW
import Lev.Internal.Recon
import Data.Functor.Indexed ((<<$>>))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BS

-- GFixed заходи через гейт Lev Fixed
-- GVariable может заходить через произвольный гейт, нужно каким-то образом определять через какой (возможно через функционалную зависимость a -> l).
-- Если сделать общий класс типов, будут возможны два варианта интерфейса:
-- 1. Общий интерфейс, эквивалентный variable. Такой интерфейс не подойдёт для Fixed, т.к. Fixed требует FixedPeek и FixedPoke.
-- 2. Интерфейс с семействами типов для возвращаемых значений. Если он будет открытый, от него не будет особого толка. Если закрытый - не получится определять
--    собственные виды сериализаторов (зато использование Generic интерфейса будет выглядеть красиво). 
-- 3. Отдельные классы типов и адаптеры для них. Тогда потребуются incoherent instances и вообще будет сложно разруливать пересечения экземпляров.

data Fixed 
data Variable

-- data FixedSize (n :: Nat) = FixedSize

data TestFixedData = TestFixedData
data TestVariableData = TestVariableData

data family Size :: Type -> Type -> Type
data instance Size Fixed a where FixedSize :: Int -> Size Fixed a
data instance Size Variable a where VariableSize :: (a -> Int) -> Size Variable a

class Lev l a  where
  size :: Size l a

instance Lev Fixed TestFixedData where 
  size :: Size Fixed TestFixedData  
  size = FixedSize 3

-- пересекающиеся по типу а экземпляры не позволяют воспользоваться функциональной зависимостью a -> l. 
-- Это в свою очередь вынуждает использовать  
instance {-# OVERLAPPABLE #-} Lev Variable a where 
  size :: Size Variable a  
  size = undefined

instance Lev Variable TestVariableData where 
  size = VariableSize $ \a -> 2

-- сериализатор: 1. что? 2. куда? 3. используя какой сериализатор?

class Serialize l s where
  type SerializeConstraint l s (m :: Type -> Type) :: Constraint
  serialize :: (Lev l a, SerializeConstraint l s m) => a -> m s

instance Serialize Fixed ByteString where
  type SerializeConstraint Fixed ByteString m = MonadIO m
  serialize :: forall a m . (Lev Fixed a, SerializeConstraint Fixed ByteString m) => a -> m ByteString
  serialize a = do 
    let (FixedSize sz) = size @Fixed @a
    liftIO $ BS.create sz $ \ptr -> return ()

instance Serialize Variable ByteString where
  type SerializeConstraint Variable ByteString m = MonadIO m
  serialize :: forall a m . (Lev Variable a, SerializeConstraint Variable ByteString m) => a -> m ByteString
  serialize a = do 
    let (VariableSize f) = size @Variable @a
    let sz = f a
    liftIO $ BS.create sz $ \ptr -> return ()

test1 :: IO ByteString
test1 = serialize @Fixed TestFixedData

test2 :: IO ByteString
test2 = serialize @Variable TestVariableData

{-
fixedSizeTest :: Size Fixed 10
fixedSizeTest = FixedSize @10

fixedSizeTestValue = fixedSizeValue fixedSizeTest  

variableSizeTest :: Size Variable Int
variableSizeTest = VariableSize $ const 42

variableSizeValue :: Size Variable a -> a -> Int
variableSizeValue (VariableSize f) = f

variableSizeTestValue = variableSizeValue variableSizeTest 10
-}

-- class Lev l a | a -> l where 
  


{-
data VariablePeek (m :: Type -> Type) a = VariablePeek 

newtype FixedPeek (m :: Type -> Type) (i :: Nat) (j :: Nat) a = Peek {unPeek :: IxMW (ReconT (Ptr Word8) m) i j a}
  deriving newtype -- newtype can be removed if removing DerivingStrategies
    ( IxFunctor,
      IxPointed,
      IxApplicative,
      IxMonad
    )

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

--- 

data Test = Test deriving (Generic, Lev Fixed)

testSize = size (undefined :: Test)
-}