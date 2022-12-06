{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Lev.Overlapping where 
import Data.Data (Proxy, typeOf, typeRep, Typeable)
import Data.Proxy (Proxy(Proxy))
import GHC.Event.Windows.FFI (OVERLAPPED)
import Debug.Trace (trace)
import Language.Haskell.TH (Overlap(Incoherent), Type (PromotedConsT))
import GHC.Exts (Any)
import Control.Monad.Identity (Identity (runIdentity))

-- https://stackoverflow.com/questions/74479429/selecting-a-type-class-instance-based-on-the-context-once-again

{-

Реализовать вычисление размера для данных типа [a]. 
Если a имеет фиксированный размер, то размер должен быть вычислен как длина списка умноженная на размер a.
Если а имеет переменный размер, то размер должен быть вчислен как сумма размеров элементов списка.

-}

-- TODO: попробовать вариант с классом типов, индексированным тегом реализации (только вот в случае списка это не сработает).



{- 
Type classes solution

class Size a where 
  size :: a -> Int

class FixedSize a where 
  fixedSize :: p a -> Int

instance FixedSize Int where 
  fixedSize _ = 8

instance (ListSize tag [a], IsFixedSize tag a) => Size [a] where 
  size = listSize (Proxy :: Proxy tag)

class ListSize (isElemFixed :: Bool) a where
  listSize :: p isElemFixed -> a -> Int

instance (FixedSize a) => ListSize 'True [a] where 
  listSize _ = trace "elem fized" . (* fixedSize (Proxy @a) ) . length

instance (Size a) => ListSize 'False [a] where 
  listSize _ = trace "elem variable" . sum . map size

class IsFixedSize tag a  | a -> tag
instance IsFixedSize 'True Int
instance {-# OVERLAPPABLE #-} (tag ~ 'False) => IsFixedSize tag a

test1 = size [1::Int,2,3]

test2 = size [[1::Int], [2,3,4]]

-}

{- Type families solution (closed)

class Size a where 
  size :: a -> Int

class FixedSize a where 
  fixedSize :: p a -> Int

instance FixedSize Int where 
  fixedSize _ = 8

class ListSize (isElemFixed :: Bool) a where
  listSize :: p isElemFixed -> a -> Int

instance (ListSize (IsFixedSize a) [a]) => Size [a] where 
  size = listSize (Proxy :: Proxy (IsFixedSize a))

instance (FixedSize a) => ListSize 'True [a] where 
  listSize _ = trace "elem fized" . (* fixedSize (Proxy @a) ) . length

instance (Size a) => ListSize 'False [a] where 
  listSize _ = trace "elem variable" . sum . map size

type family IsFixedSize a where 
  IsFixedSize Int = 'True
  IsFixedSize a = 'False

test1 = size [1::Int,2,3]

test2 = size [[1::Int], [2,3,4]]
 -}

-- Type families solution (open + incoherent instances) 

{-

Selecting a type-class instance based on the context (once again).

Ok this topic had been discussed a lot of times, but since Haskell evolves lets consider it again to see what we can do in contemporary Haskell (by contemporary I mean GHC 9.0 - 9.2).

Fist let me state the problem by an example. Suppose we have a function which determines a number of bytes required to store a value of a given type. We can have two instances of this function: one for fixed sized data types and other for variable sized. For example `Int32` is fixed sized and always takes 4 bytes to store regardless it's value. But `data C = A Int32 | B Int32 Int32` can be considered variable sized since it may take 4 bytes to store in case of `A` constructor or 8 bytes in case of `B` constructor. It's natural to have two classes for this:

1. A class for fixed sized values. Note that value itself not required, we can use `Proxy` as a parameter to determine the size.
```haskell
class FixedSize a where 
  fixedSize :: p a -> Int
```

2. A class for variable sized values. The function takes a value to determine the size.
```haskell
class VariableSize a where 
  variableSize :: a -> Int
```

Now lets say we want to define a function which determines the size of a list of values. The values in the list can be either fixed or variable sized. So it's natural to have two functions:

1. One for a list of fixed sized values.
```haskell
listSize :: (FixedSize a) => [a] -> Int
listSize _ = (* fixedSize (Proxy @a) ) . length
```

2. Other for a list of variable sized values.
```haskell
listSize :: (VariableSize a) => [a] -> Int
listSize = sum . map variableSize
```

However it is not possible to use a naive approach, the following basically won't compile:
```haskell
class Size a where 
  size :: a -> Int

instance (FixedSize a) => Size [a] where
  size = _ = (* fixedSize (Proxy @a) ) . length

instance (VariableSize a) => Size [a] where
  size = sum . map variableSize
```

This happens because Haskell relies on type when selecting an instance, but not on the context. There are trick to overcome this limitation described here: https://wiki.haskell.org/GHC/AdvancedOverlap. The basic idea is to define type-level predicate which reflects the context and use it to select an instance using multi-parameter type classes an overlapping instances. In this case Haskell will be able to select more specific instance based on the type parameters. By "more specific" I mean matching type-level predicates. 

The proposed approaches can be divided into three groups conditionally.

1. Use closed type families to define a type-level predicate ("Solution 3" according to the wiki-page). This is not usable approach because it will disallow user to define instances for custom data types. I won't discuss it further.

2. Define the predicate as a separate type class, define default (fallback) overlappable instance for the predicate ("Solution 1" according to the wiki-page). This is working approach, but it requres from user to maintain additional instances for the predicate.

3. Use open type families ("Solution 2"). I'd like to discuss slightly modified version of this approach. 

```haskell
class Size a where 
  size :: a -> Int

class FixedSize a where
  type FixedSized a :: Bool
  type FixedSized a = 'True
  fixedSize :: p a -> Int

#include "MachDeps.h"
instance FixedSize Int where
  fixedSize _ = SIZEOF_HSINT

class ListSize (isElemFixed :: Bool) a where
  listSize :: p isElemFixed -> a -> Int

instance (ListSize (FixedSized a) [a]) => Size [a] where
  size = listSize $ Proxy @(FixedSized a)

instance (FixedSize a) => ListSize 'True [a] where
  listSize _ = trace "elem size is fiхed" . (* fixedSize (Proxy @a) ) . length

instance {-# INCOHERENT #-} (Size a) => ListSize any [a] where
  listSize _ = trace "elem size is variable" . sum . map size

test1 = size [1::Int,2,3]

test2 = size [[1::Int], [2,3,4]]
```

This approach seems the most convenient user-wise to me. The separate type-level predicate facility is still required and user can still mess up by defining something like this explicitly:
```haskell
class FixedSize UserType where
  type FixedSized UserType = 'False
```
but it just works as expected when using defaults. 

However, it reqires incoherent instances. And I'm scare of incoherent instances. Because the Hasllkel documentation leterelly says that in case of incoherent instances the compiler is free to choose any instance it wants which looks unpredictable. Now I'll probably doing a bad thing by asking 4 questions in one post but they all related:

1. Why incoherent instances are needed here exactly? Does not `ListSize 'True [a]` just overlap with `ListSize any [a]` and could be picked when first paramenter evaluates to `True`?

2. Is there a way to break this code? I mean, to make a complier to choose `ListSize any [a]` (variable sized elem code) when `FixedSize a` is in scope?

3. Are these instances really incoherent? Probably compiler just can't prove coherence, so how it can be proven manually?

4. Is there a completely different approach to solve this problem in modern Haskell? By the problem I mean a partial exapmle above, selecting an appropriate function to determine the size of a list of values based on the type of the values in compile time.






I think it's worth to take a look at it again. The problem is that we want to have a more specific implementations or more generic and we want to make a Haskell compiler to pick a more specific one leaving a more generic one as a fallback. However it's doesn't work in Haskell. It's designed to be rigid and type-predictable. There is a some tricks to bypass Haskell's limitations described here https://wiki.haskell.org/GHC/AdvancedOverlap maybe outdated, but still relevant. Consider following task: you have to calculate the size of list, depending 

-}

class Size a where 
  size :: a -> Int

class FixedSize a where
  type FixedSized a :: Bool
  type FixedSized a = 'True
  fixedSize :: p a -> Int

#include "MachDeps.h"
instance FixedSize Int where
  fixedSize _ = SIZEOF_HSINT

class ListSize (isElemFixed :: Bool) a where
  listSize :: p isElemFixed -> a -> Int

instance (ListSize (FixedSized a) [a]) => Size [a] where
  size = listSize $ Proxy @(FixedSized a)

instance (FixedSize a) => ListSize 'True [a] where
  listSize _ = trace "elem size is fiхed" . (* fixedSize (Proxy @a) ) . length

instance {-# INCOHERENT #-} (Size a) => ListSize any [a] where
  listSize _ = trace "elem size is variable" . sum . map size
