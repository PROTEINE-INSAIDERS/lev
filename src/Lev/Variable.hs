{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{--# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-
OVERLAPPING  - позволяет перегрузить болле общий экземпляр более специфичным (устанавливается на более специфичном экземпляре).
OVERLAPPABLE - разрешает перегружать более общий экземпляр более специфичнымы (устанавливается на более общем экземпляре).
OVERLAPS - сочетает в себе OVERLAPPING и OVERLAPPABLE.
-}

module Lev.Variable where 
import Data.Word (Word8)
import Data.Data (Proxy (Proxy))

import GHC.Generics (Generic (Rep, from), M1 (M1), D, C, S, type (:*:) ((:*:)), K1 (K1), U1, type (:+:))
import GHC.TypeLits (Nat, type (+), natVal, KnownNat)
import Data.Kind (Type)
import Lev.Internal.Fixed.Core (Max)
import qualified Flat as Lev
-- import Data.Fixed (Fixed)

-- | Parser type tags.
-- Using kind 'Type' instead of lifted sum type to allow custom tags. 
data Fixed
data Variable  

class Lev (a :: Type) where
  size :: a -> Int

instance {-# OVERLAPPABLE #-} (LevImpl l a) => Lev a where 
  size = sizeImpl (Proxy @l)

-- идеи:
-- - сериализация мутаблельных структур - это возможно, для этого потребуется, чтобы size запускался в монаде. Это не выглядит чем-то сложным.
-- - для фиксированных типов не требуется значения a, следует ли создать тип данных size как в Store? 

class LevImpl l a | a -> l where
  sizeImpl :: p l -> a -> Int
  default sizeImpl :: (Generic a, GSize l (Rep a)) => p l -> a -> Int 
  sizeImpl p = gsize p . from

instance LevImpl Fixed Word8 where
  sizeImpl _ _ = 1

class GSize l f where
  gsize :: p l -> f a -> Int

instance (GSize Variable f) => GSize Variable (M1 ti c f) where
  gsize p (M1 x) = gsize p x

instance GSize Variable U1 where
  gsize _ = const 0

instance (LevImpl l a) => GSize Variable (K1 i a) where
  gsize _ (K1 a) = size a

instance (GSize Variable f, GSize Variable g) => GSize Variable (f :*: g) where
  gsize p (x :*: y) = gsize p x + gsize p y

instance LevImpl Fixed a => LevImpl Variable [a] where 
  sizeImpl _ b = size (undefined :: a) * length b


data TestData = TestData Word8 [Word8] deriving (Generic, LevImpl Variable)

-- test = size (TestData 3 [1, 2 ,3])

{-
class (KnownNat (FixedSize (a :: Type))) => Fixed a where -- сможем ли мы этим воспользоваться, оставив это только в контексте класса? 
  type FixedSize a :: Nat  
  type FixedSize a = GFixedSize (Rep a)
  type IsFixed a :: Bool
  type IsFixed a = 'True -- возможно следует сделать функцию для generic определения. 
  
fixedSize :: forall a. (Fixed a) => Proxy a -> Int
fixedSize _ = fromInteger $ natVal (Proxy :: Proxy (FixedSize a))


instance Fixed Word8 where
  type FixedSize Word8 = 1

type family GFixedSize (f :: Type -> Type) :: Nat where
  GFixedSize (M1 ti c f) = GFixedSize f
  GFixedSize (f :*: g) = GFixedSize f + GFixedSize g
  GFixedSize (f :+: g) = FixedSize Word8 + Max (GFixedSize f) (GFixedSize g)
  GFixedSize (K1 i c) = FixedSize c 
  GFixedSize U1 = 0

{- 
Задача: 
Есть 2 вида сериализаторов - для данных фиксированного и переменнтого размера. Сериализаторы имеют реализации по умолчанию, использующие 
обобщенные представления. Необходимо предоставить возможность использования сериализатора данных фиксированного размера для полей данных 
переменного размера.

Решение:
Решение основано на перекрывающихся экземплярах с использованием техники, описанной в статье https://wiki.haskell.org/GHC/AdvancedOverlap
1. В качестве фасада используется класс Lev a (на самом деле он не особенно нужен, он просто позволяет спрятать флаг указывающий на тип сериализатора).
   Он имеет один экземпляр по-умолчанию, который матчится всегда и перенаправляет вызовы в класс-переключатель (вместо переключателя можно использовать
   реализацию по-умолчанию, это позволит перегружать класс без использования перекрывающихся экземпляров).

2. Создаётся класс-преключатель LevSwitch flag a. У него два неперекрывающихся экземпляра - для сериализаторов фиксированного и переменного размеров.
   Выбор зависит от предиката UseFixed type.

3. В обобщенной реализации при сериализации/десериализации производится обращение к фасадному классу.

Момент, который следует проверить. 
Обобщенная реализация потребует наследования как Fixed, так и UseFixed. В случае если фасадный Lev будет реализован не через экземпляр по-умолчанию, 
а с использованием метода по-умолчанию, то придётся наследовать еще и Lev.PostAligned

Сдедует ли из наличия Fixed в текущей области то, что он должен использоваться при построении сериализатора?
- В пользу такого поведения тот факт, что если какой либо экземпляр находится в текущей области, то он работает.
- Против - то, что сериализация работает через экземпляры Lev, а к Fixed мы приходим неявно. С другой стороны,
  UseFixed это тоже не Lev, а наличие выделенного экземпляра Lev перекроет его. 
-}
{-
Эксперементальный вариант:
Используется единственный класс Lev f a где f - флаг, указывающий на тип сериализатора. При вызове из generic сериализатора используется 
переключатель с двумя экземплярами.
-}
-}