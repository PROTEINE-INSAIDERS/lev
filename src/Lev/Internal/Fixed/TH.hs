{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Lev.Internal.Fixed.TH where

import Control.Monad (liftM2)
import Control.Monad.Indexed (ireturn)
import Control.Monad.Primitive (PrimMonad)
import Data.Functor.Indexed ((*>>), (<<$>>), (<<*>>))
import Data.List (mapAccumL)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Primitive (Prim (..))
import qualified Data.Set as Set
import GHC.TypeLits (KnownNat)
import GHC.TypeNats (type (+))
import GHC.Types (Int (I#))
import Language.Haskell.TH (Body (NormalB), Con (NormalC), Cxt, Dec (DataD, InstanceD, TySynInstD), Exp (AppE, CaseE, ConE, LamE, UInfixE, VarE), Info (ClassI, TyConI), Match (Match), Name, Pat (ConP, VarP), Q, TyLit (NumTyLit), TySynEqn (TySynEqn), TyVarBndr (KindedTV, PlainTV), Type (AppT, ConT, InfixT, LitT, TupleT, UInfixT, VarT), listE, litT, mkName, newName, normalB, numTyLit, pprint, reify, sigD, tupleT, valD, varP, varT)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Syntax (Quote)
import Lev.Internal.Fixed.Core
import TH.ReifySimple
  ( DataType (DataType, dtCons, dtCxt, dtName, dtTvs),
    reifyDataType,
  )
import TH.Utilities (appsT)

-- | Primitive types sizes (used to enerate Serialize instances for them)
$( do
     let name = mkName "primitiveTypeSizes"
     sign <- sigD name [t|Map Type Int|]
     info <- reify ''Prim
     let types = case info of
           ClassI _ instnces -> [ty | InstanceD _ _ (AppT _ ty@(ConT _)) _ <- instnces]
           _ -> error "mkPrimitiveTypeSizes: expected ClassI"
     let typeSizes = map (\ty -> [e|(ty, I# (sizeOf# (undefined :: $(return ty))))|]) types
     val <- valD (varP name) (normalB [|Map.fromList $(listE typeSizes)|]) []
     return [sign, val]
 )

mkPrimitiveSerialize :: Name -> Q [Dec]
mkPrimitiveSerialize name = do
  DataType {..} <- reifyDataType name
  let a = appsT (ConT name) (map VarT dtTvs) -- this is not really required for primitive types
  mkPrimitiveSerialize' a (toInteger $ primitiveTypeSizes Map.! a)

mkPrimitiveSerialize' :: (Quote m) => Type -> Integer -> m [Dec]
mkPrimitiveSerialize' a s =
  let m = varT $ mkName "m"
      o = varT $ mkName "o"
      size = litT $ numTyLit s
      constraint = [t|(PrimMonad $m, KnownNat $o)|]
      peekExpr = [|peekPrim|]
      pokeExpr = [|pokePrim|]
   in makeSerializeInstance (return []) m o (return a) size constraint peekExpr pokeExpr

mkPrimitiveSerializeInsances :: Q [Dec]
mkPrimitiveSerializeInsances =
  concat <$> mapM (uncurry mkPrimitiveSerialize') (Map.toList $ Map.map toInteger primitiveTypeSizes)

-- | Strip out duplicates (taken from RIO) TODO: remove
nubOrd :: Ord a => [a] -> [a]
nubOrd =
  loop mempty
  where
    loop _ [] = []
    loop !s (a : as)
      | a `Set.member` s = loop s as
      | otherwise = a : loop (Set.insert a s) as

tyVarBndrName :: TyVarBndr a -> Name
tyVarBndrName (PlainTV n _) = n
tyVarBndrName (KindedTV n _ _) = n

makeSerialize :: Name -> Q [Dec]
makeSerialize = serializeInstanceD

makeSerializeInstance :: Quote m => m Cxt -> m Type -> m Type -> m Type -> m Type -> m Type -> m Exp -> m Exp -> m [Dec]
makeSerializeInstance ctx m o a size constraint peekExpr pokeExpr =
  [d|
    instance Serialize $m $o $a where
      type SizeOf $a = $size
      type SerializeConstraint $m $o $a = $constraint
      {-# INLINE peek #-}
      peek = $peekExpr
      {-# INLINE poke #-}
      poke = $pokeExpr
    |]
    `appendInstancesCxtQ` ctx

appendInstancesCxtQ :: (Monad m) => m [Dec] -> m Cxt -> m [Dec]
appendInstancesCxtQ = liftM2 $ \ds c -> map (`appendInstanceCxt` c) ds
  where
    appendInstanceCxt (InstanceD o c ts ds) c' = InstanceD o (c ++ c') ts ds
    appendInstanceCxt d _ = d

serializeInstanceD :: Name -> Q [Dec]
serializeInstanceD aName = do
  info <- reify aName
  case info of
    TyConI (DataD _ _ vars _ [NormalC cName bangTypes] _) -> do
      -- TODO: add newtype support, multi-constructor support, newtype support (use th utils?)
      m <- VarT <$> newName "m"
      o <- VarT <$> newName "o"
      -- aType <- foldM (\a b -> AppT a <$> b) (ConT aName) (map (\n -> VarT <$> newName (show $ tyVarBndrName n)) vars)

      let aType = foldl AppT (ConT aName) (map (VarT . tyVarBndrName) vars) -- TODO: capture tyVarBndrName
          fieldTypes = map snd bangTypes

          sizeOfT = case fieldTypes of
            [] -> LitT $ NumTyLit 0
            -- _ -> foldl (\a t -> UInfixT a ''(+) (ConT ''SizeOf `AppT` t)) (LitT $ NumTyLit 0) fieldTypes
            _ -> foldl1 (\a t -> UInfixT a ''(+) t) $ map (AppT $ ConT ''SizeOf) fieldTypes

          sizeOfD =
            TySynInstD $
              TySynEqn Nothing (ConT ''SizeOf `AppT` aType) sizeOfT
          serializeConstraintT off ty = ConT ''SerializeConstraint `AppT` m `AppT` off `AppT` ty

          serializeConstraintRHS = case fieldTypes of
            [] -> TupleT 0
            [ty] -> serializeConstraintT o ty -- Typle of one element expanded to "Solo" by HLS, still supported normally by GHC
            _ ->
              -- TODO: сначала получить список констрейнтов, потом получить кортеж.
              let (offsetAndSize, constraints) =
                    -- тут в fst будет смещение и сумма всех размеров
                    mapAccumL
                      ( \off ty ->
                          ( UInfixT off ''(+) (ConT ''SizeOf `AppT` ty),
                            serializeConstraintT off ty
                          )
                      )
                      o
                      fieldTypes
               in foldl AppT (TupleT $ length fieldTypes + 1) (UInfixT (InfixT o ''(+) sizeOfT) ''(~) offsetAndSize : constraints)
          -- in fst $ foldl (\(cx, off) ty -> (cx `AppT` serializeConstraintT off ty, UInfixT off ''(+) (ConT ''SizeOf `AppT` ty))) (TupleT $ length fieldTypes, o) fieldTypes

          serializeConstraintD = TySynInstD $ TySynEqn Nothing (serializeConstraintT o aType) serializeConstraintRHS

      fields <-
        mapM
          ( \(i, ty) -> do
              n <- newName $ "a" ++ show i
              return (n, ty)
          )
          (zip [0 ..] fieldTypes)

      let pokeP = ConP cName $ map (VarP . fst) fields
      let pokeB = NormalB $ case fields of
            [] -> AppE (VarE 'ireturn) (ConE '())
            -- [] -> AppE (VarE 'ireturn) (ConE '())
            -- _ -> foldl (\a (n, ty) -> UInfixE (VarE '(*>>)) (VarE 'poke)  (VarE 'poke)     ) (AppE (VarE 'ireturn) (ConE '())) fields
            _ -> foldr (\(n, ty) a -> UInfixE (VarE 'poke `AppE` VarE n) (VarE '(*>>)) a) (AppE (VarE 'ireturn) (ConE '())) fields
      -- [(n, ty)] -> VarE 'poke `AppE` VarE n `AppE` VarE 'undefined -- TODO: use offset
      let valName = mkName "val"
          pokeMatch = Match pokeP pokeB []
          pokeExpr = LamE [VarP valName] $ CaseE (VarE valName) [pokeMatch]

      let peekExpr = case fields of
            [] -> AppE (VarE 'ireturn) (ConE cName)
            _ -> fst $ foldl (\(a, op) _ -> (UInfixE a op (VarE 'peek), VarE '(<<*>>))) (ConE cName, VarE '(<<$>>)) [0 .. length fields - 1]

      makeSerializeInstance (return []) (return m) (return o) (return aType) (return sizeOfT) (return serializeConstraintRHS) (return peekExpr) (return pokeExpr)
    -- return $ InstanceD Nothing [] (ConT ''Serialize `AppT` m `AppT` o `AppT` aType) [sizeOfD, serializeConstraintD, pokeD, pokeID, peekD, peekID]
    _ -> fail $ "Expected to reify a datatype, instead got:\n" ++ pprint info
