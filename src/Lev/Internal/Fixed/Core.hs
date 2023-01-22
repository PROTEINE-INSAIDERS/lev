{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Lev.Internal.Fixed.Core where

import Control.Monad.Indexed (IxApplicative (iap), IxFunctor, IxMonad (ibind), IxPointed, ireturn, (>>>=))
import Control.Monad.Primitive (PrimMonad)
import Data.Data (Proxy (Proxy))
import Data.Functor.Indexed (IxApplicative, IxFunctor (imap), (*>>), (<<$>>), (<<*>>))
import Data.Kind (Constraint, Type)
import Data.Primitive (Prim)
import Data.Primitive.Ptr (readOffPtr, writeOffPtr)
import Data.Word (Word8)
import Foreign (Ptr, plusPtr)
import GHC.Generics (C1, Generic (from, to), K1 (K1), M1 (M1), Rep, U1 (U1), type (:*:) ((:*:)), type (:+:) (L1, R1))
import GHC.TypeLits (CmpNat, ErrorMessage (Text), Nat, TypeError, natVal, type (+), type (-), type (<=?))
import GHC.TypeNats (KnownNat)
import Lev.Internal.IxMW
import Lev.Internal.Recon

newtype Peek (m :: Type -> Type) (i :: Nat) (j :: Nat) a = Peek {unPeek :: IxMW (ReconT (Ptr Word8) m) i j a}
  deriving
    ( IxFunctor,
      IxPointed,
      IxApplicative,
      IxMonad
    )

{-# INLINE runPeek #-}
runPeek :: (Monad m) => Peek m i j a -> Ptr Word8 -> m a
runPeek p ptr = runReconT (unIxMW $ unPeek p) ptr return

{-# INLINE peekPrim #-}
peekPrim :: forall m i j a. (KnownNat i, PrimMonad m, Prim a) => Peek m i j a
peekPrim = Peek $
  IxMW $ do
    ptr <- ask
    lift $ readOffPtr (ptr `plusPtr` off) 0
  where
    off = fromInteger $ natVal (Proxy :: Proxy i)

{-# INLINE peekSkip #-}
peekSkip :: forall m i s. Proxy s -> Peek m i (i + s) ()
peekSkip _ = Peek $ IxMW $ return ()

newtype Poke m (i :: Nat) (j :: Nat) a = Poke {unPoke :: IxMW (ReconT (Ptr Word8) m) i j a}
  deriving
    ( IxFunctor,
      IxPointed,
      IxApplicative,
      IxMonad
    )

{-# INLINE runPoke #-}
runPoke :: (Monad m) => Poke m i j a -> Ptr Word8 -> m a
runPoke p ptr = runReconT (unIxMW $ unPoke p) ptr return

{-# INLINE pokePrim #-}
pokePrim :: forall m i j a. (KnownNat i, PrimMonad m, Prim a) => a -> Poke m i j ()
pokePrim a = Poke $
  IxMW $ do
    ptr <- ask
    lift $ writeOffPtr (ptr `plusPtr` off) 0 a
  where
    off = fromInteger $ natVal (Proxy :: Proxy i)

{-# INLINE pokeSkip #-}
pokeSkip :: forall m i s. Proxy s -> Poke m i (i + s) ()
pokeSkip _ = Poke $ IxMW $ return ()

-- TODO: нужно попробовать убрать параметры m o из класса, фактически они могут быть перенесены в контекст.
-- также следует переименовать Constraint в Context.
-- Возможно следует разбить класс на подклассы: FixedSize, FixedPeek, FixedPoke.
-- На верхнем уровне возможно такое разбиение не имеет смысла, но на уровне генериков контексты Peek и Poke отличаются для типов-сумм.
class Serialize (m :: Type -> Type) o a where
  type SizeOf a :: Nat
  type SizeOf a = GSizeOf (Rep a)

  type SerializeConstraint m o a :: Constraint
  type SerializeConstraint m o a = GSerializeConstraint m o (Rep a)

  {-# INLINE peek #-}
  peek :: (SerializeConstraint m o a) => Peek m o (o + SizeOf a) a
  default peek ::
    ( Generic a,
      GSerialize m o (Rep a),
      GSerializeConstraint m o (Rep a),
      SizeOf a ~ GSizeOf (Rep a)
    ) =>
    Peek m o (o + SizeOf a) a
  peek = to <<$>> gPeek

  {-# INLINE poke #-}
  poke :: (SerializeConstraint m o a) => a -> Poke m o (o + SizeOf a) ()
  default poke ::
    ( Generic a,
      GSerialize m o (Rep a),
      GSerializeConstraint m o (Rep a),
      SizeOf a ~ GSizeOf (Rep a)
    ) =>
    a ->
    Poke m o (o + SizeOf a) ()
  poke = gPoke . from

-- TODO: возможно стоит дать более специфичное имя
sizeOf :: forall a f. (KnownNat (SizeOf a)) => f a -> Int
sizeOf _ = fromInteger $ natVal (Proxy :: Proxy (SizeOf a))

class GSerialize m o f where
  type GSizeOf f :: Nat
  type GSerializeConstraint m o f :: Constraint

  gPeek :: (GSerializeConstraint m o f) => Peek m o (o + GSizeOf f) (f a)
  gPoke :: (GSerializeConstraint m o f) => f a -> Poke m o (o + GSizeOf f) ()

instance GSerialize m o f => GSerialize m o (M1 ti c f) where
  type GSizeOf (M1 ti c f) = GSizeOf f
  type GSerializeConstraint m o (M1 ti c f) = GSerializeConstraint m o f
  {-# INLINE gPeek #-}
  gPeek = M1 <<$>> gPeek
  {-# INLINE gPoke #-}
  gPoke (M1 x) = gPoke x

instance GSerialize m o U1 where
  type GSizeOf U1 = 0
  type GSerializeConstraint m o U1 = ()
  {-# INLINE gPeek #-}
  gPeek = ireturn U1
  {-# INLINE gPoke #-}
  gPoke = const $ ireturn ()

instance (Serialize m o c) => GSerialize m o (K1 ti c) where
  type GSizeOf (K1 ti c) = SizeOf c
  type GSerializeConstraint m o (K1 ti c) = (SerializeConstraint m o c)
  {-# INLINE gPeek #-}
  gPeek = K1 <<$>> peek
  {-# INLINE gPoke #-}
  gPoke (K1 x) = poke x

instance (GSerialize m o f, GSerialize m (o + GSizeOf f) g) => GSerialize m o (f :*: g) where
  type GSizeOf (f :*: g) = GSizeOf f + GSizeOf g
  type
    GSerializeConstraint m o (f :*: g) =
      ( GSerializeConstraint m o f,
        GSerializeConstraint m (o + GSizeOf f) g,
        o + GSizeOf f + GSizeOf g ~ o + (GSizeOf f + GSizeOf g)
      )
  {-# INLINE gPeek #-}
  gPeek = (:*:) <<$>> gPeek <<*>> gPeek
  {-# INLINE gPoke #-}
  gPoke (x :*: y) = gPoke x *>> gPoke y

type family OrdCond o lt eq gt where
  OrdCond 'LT lt eq gt = lt
  OrdCond 'EQ lt eq gt = eq
  OrdCond 'GT lt eq gt = gt

type Max m n = OrdCond (CmpNat m n) n n m

instance
  ( Serialize m o Word8,
    SerializeConstraint m o Word8,
    FitsInByte (SumArity (f :+: g)),
    GPeekSum m (o + SizeOf Word8) 0 (f :+: g),
    GPokeSum m o 0 (f :+: g)
  ) =>
  GSerialize m o (f :+: g)
  where
  type GSizeOf (f :+: g) = SizeOf Word8 + GPeekSumSizeOf (f :+: g)
  type
    GSerializeConstraint m o (f :+: g) =
      ( GPeekSumConstraint m (o + SizeOf Word8) (f :+: g),
        o + SizeOf Word8 + GPeekSumSizeOf (f :+: g) ~ o + (SizeOf Word8 + GPeekSumSizeOf (f :+: g)),
        GPokeSumConstraint m o (f :+: g),
        o + GPokeSumSizeOf (f :+: g) ~ o + (SizeOf Word8 + GPeekSumSizeOf (f :+: g))
      )
  {-# INLINE gPeek #-}
  gPeek = peek >>>= \tag -> gPeekSum tag $ Proxy @0
  {-# INLINE gPoke #-}
  gPoke x = gPokeSum x (Proxy :: Proxy 0)

type family SumArity a where
  SumArity (C1 c a) = 1
  SumArity (x :+: y) = SumArity x + SumArity y

type FitsInByte n = FitsInByteResult (n <=? 255)

type family FitsInByteResult (b :: Bool) :: Constraint where
  FitsInByteResult 'True = ()
  FitsInByteResult 'False = TypeError ('Text "Generic deriving of Serialize instances can only be used on datatypes with fewer than 256 constructors.")

class GPeekSum m o n f where
  type GPeekSumSizeOf f :: Nat
  type GPeekSumConstraint m o f :: Constraint
  gPeekSum :: (GPeekSumConstraint m o f) => Word8 -> Proxy n -> Peek m o (o + GPeekSumSizeOf f) (f a)

instance (GPeekSum m o n f, GPeekSum m o (n + SumArity f) g, KnownNat (n + SumArity f)) => GPeekSum m o n (f :+: g) where
  type GPeekSumSizeOf (f :+: g) = Max (GPeekSumSizeOf f) (GPeekSumSizeOf g)
  type
    GPeekSumConstraint m o (f :+: g) =
      ( GPeekSumConstraint m o f,
        GPeekSumConstraint m o g,
        o + GPeekSumSizeOf (f :+: g) ~ o + GPeekSumSizeOf f + (GPeekSumSizeOf (f :+: g) - GPeekSumSizeOf f),
        o + GPeekSumSizeOf (f :+: g) ~ o + GPeekSumSizeOf g + (GPeekSumSizeOf (f :+: g) - GPeekSumSizeOf g)
      )

  {-# INLINE gPeekSum #-}
  gPeekSum tag proxyL -- peekSkip используется чтобы пропустить остаток данных, если размер знечения меньше максимального размера.
    | tag < sizeL = gPeekSum tag proxyL >>>= \x -> peekSkip Proxy *>> ireturn (L1 x)
    | otherwise = gPeekSum @m @o @(n + SumArity f) @g tag (Proxy @(n + SumArity f)) >>>= \x -> peekSkip Proxy *>> ireturn (R1 x)
    where
      sizeL = fromInteger (natVal (Proxy :: Proxy (n + SumArity f)))

instance (KnownNat n, GSerialize m o f) => GPeekSum m o n (M1 ti c f) where
  type GPeekSumSizeOf (M1 ti c f) = GSizeOf f
  type GPeekSumConstraint m o (M1 ti c f) = (GSerializeConstraint m o f)

  {-# INLINE gPeekSum #-}
  gPeekSum tag _
    | tag == cur = gPeek
    | tag > cur = error "Sum tag invalid"
    | otherwise = error "Error in implementation of Lev Generics"
    where
      cur = fromInteger (natVal (Proxy :: Proxy n))

class GPokeSum m o n f where
  type GPokeSumSizeOf f :: Nat
  type GPokeSumConstraint m o f :: Constraint

  gPokeSum :: (GPokeSumConstraint m o f) => f a -> Proxy n -> Poke m o (o + GPokeSumSizeOf f) ()

instance (GPokeSum m o n f, GPokeSum m o (n + SumArity f) g) => GPokeSum m o n (f :+: g) where
  type GPokeSumSizeOf (f :+: g) = Max (GPokeSumSizeOf f) (GPokeSumSizeOf g)
  type
    GPokeSumConstraint m o (f :+: g) =
      ( GPokeSumConstraint m o f,
        GPokeSumConstraint m o g,
        o + GPokeSumSizeOf (f :+: g) ~ o + GPokeSumSizeOf f + (GPokeSumSizeOf (f :+: g) - GPokeSumSizeOf f),
        o + GPokeSumSizeOf (f :+: g) ~ o + GPokeSumSizeOf g + (GPokeSumSizeOf (f :+: g) - GPokeSumSizeOf g)
      )

  {-# INLINE gPokeSum #-}
  gPokeSum (L1 l) _ = gPokeSum l (Proxy :: Proxy n) *>> pokeSkip Proxy
  gPokeSum (R1 r) _ = gPokeSum r (Proxy :: Proxy (n + SumArity f)) *>> pokeSkip Proxy

instance (KnownNat n, GSerialize m (o + SizeOf Word8) f, Serialize m o Word8, SerializeConstraint m o Word8) => GPokeSum m o n (M1 ti c f) where
  type GPokeSumSizeOf (M1 ti c f) = SizeOf Word8 + GSizeOf f
  type
    GPokeSumConstraint m o (M1 ti c f) =
      ( GSerializeConstraint m (o + SizeOf Word8) f,
        o + (SizeOf Word8 + GSizeOf f) ~ (o + SizeOf Word8) + GSizeOf f
      )

  {-# INLINE gPokeSum #-}
  gPokeSum x _ = poke (fromInteger (natVal (Proxy :: Proxy n)) :: Word8) *>> gPoke x
