{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Lev.Bench where

import Control.DeepSeq (NFData)
import Criterion (Benchmark, bench, bgroup, env, nf, nfIO)
import Criterion.Main (defaultMain)
import qualified Data.ByteString as BS
import Data.Int (Int32, Int64, Int8)
import Data.Store (Store)
import qualified Data.Store as Store
import qualified Data.Store.TH as Store
import Data.Typeable (Typeable, typeOf)
import Data.Word (Word8, Word32)
import Flat (Flat)
import qualified Flat
import GHC.Generics (Generic)
import GHC.TypeLits (KnownNat)
import Lev.Fixed
  ( Serialize,
    SerializeConstraint,
    SizeOf,
  )
import qualified Lev.Fixed as Lev
import UnliftIO ()

data SmallProductG = SmallProductG Int32 Int32 Int32 Int32
  deriving (Generic, Show, Typeable, NFData, Store, Serialize m o, Flat)

-- Actually Flat does not have TH. Flat instance added here for uniformity but it will use the generic instance.
data SmallProductTH = SmallProductTH Int32 Int32 Int32 Int32
  deriving (Generic, Show, Typeable, NFData, Flat)

-- todo: check store only generic 

Store.makeStore ''SmallProductTH

Lev.makeSerialize ''SmallProductTH

data SomeData = SomeData !Int64 !Word8 !Double
  deriving (Generic, Show, Typeable, NFData, Store, Serialize m o, Flat)

data SmallSum
    = SS1 Int8
    | SS2 Int32
    | SS3 Int64
    | SS4 Word32
    deriving (Generic, Show, Typeable, NFData, Store, Serialize m o, Flat)

type Ctx a = (NFData a, Store a, Serialize IO 0 a, Typeable a, SerializeConstraint IO 0 a, KnownNat (SizeOf a), Flat a)

benchEncode :: Ctx a => a -> Benchmark
benchEncode = benchEncode' ""

benchEncode' :: Ctx a => String -> a -> Benchmark
benchEncode' msg x0 =
  env (return x0) $ \x ->
    let label = msg ++ " (" ++ show (typeOf x0) ++ ")"
        benchStore name = bench name (nf Store.encode x)
        benchLev name = bench name (nfIO $ Lev.encode x)
        benchFlat name = bench name (nf Flat.flat x)
     in bgroup
          label
          [ benchLev "lev",
            benchStore "store",
            benchFlat "flat"
          ]

benchDecode :: Ctx a => a -> Benchmark
benchDecode = benchDecode' ""

benchDecode' :: forall a. Ctx a => String -> a -> Benchmark
benchDecode' prefix x0 =
  bgroup
    label
    [ env (Lev.encode x0) $ \x -> bench "lev" (nfIO (Lev.decode x :: IO a)),
      env (return (Store.encode x0)) $ \x -> bench "store" (nf (Store.decodeEx :: BS.ByteString -> a) x),
      env (return (Flat.flat x0)) $ \x -> bench "flat" (nf (ensureRight . Flat.unflat :: BS.ByteString -> a) x)
    ]
  where
    label = prefix ++ " (" ++ show (typeOf x0) ++ ")"
    ensureRight (Left e) = error $ "left!: " ++ show e
    ensureRight (Right x) = x

mainBench :: IO ()
mainBench =
  let is = 0 :: Int
      sds = SomeData 1 1 1
      smallprodsG = SmallProductG 0 1 2 3
      smallprodsTH = SmallProductTH 0 1 2 3
      sss = [SS1 1, SS2 2, SS3 3, SS4 4]
   in defaultMain
        [ bgroup
            "encode"
            [ benchEncode is,
              benchEncode sds,
              benchEncode smallprodsG,
              benchEncode smallprodsTH,
              benchEncode (head sss)
            ], 
          bgroup
            "decode"
            [ benchDecode is,
              benchDecode sds,
              benchDecode smallprodsG,
              benchDecode smallprodsTH,
              benchDecode (head sss)
            ]
        ]