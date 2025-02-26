{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.TabularFun
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.TabularFun
  ( type (=->) (..),
  )
where

import Control.DeepSeq (NFData, NFData1)
import Data.Bifunctor (Bifunctor (second))
import qualified Data.Binary as Binary
import Data.Bytes.Serial (Serial (deserialize, serialize))
import Data.Hashable (Hashable)
import qualified Data.SBV as SBV
import qualified Data.SBV.Dynamic as SBVD
import qualified Data.Serialize as Cereal
import GHC.Generics (Generic, Generic1)
import Grisette.Internal.Core.Data.Class.Function
  ( Apply (FunType, apply),
    Function ((#)),
  )
import Grisette.Internal.SymPrim.FunInstanceGen (supportedPrimFunUpTo)
import Grisette.Internal.SymPrim.Prim.Internal.PartialEval (totalize2)
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( NonFuncPrimConstraint,
    NonFuncSBVRep (NonFuncSBVBaseType),
    PEvalApplyTerm (pevalApplyTerm, sbvApplyTerm),
    SBVRep (SBVType),
    SupportedNonFuncPrim (conNonFuncSBVTerm, withNonFuncPrim),
    SupportedPrim
      ( conSBVTerm,
        defaultValue,
        parseSMTModelResult,
        pevalITETerm,
        withPrim
      ),
    SupportedPrimConstraint (PrimConstraint),
    Term,
    applyTerm,
    conTerm,
    partitionCVArg,
    pevalEqTerm,
    pevalITEBasicTerm,
    pattern ConTerm,
    pattern ITETerm,
  )
import Language.Haskell.TH.Syntax (Lift)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim

-- |
-- Functions as a table. Use the `#` operator to apply the function.
--
-- >>> let f = TabularFun [(1, 2), (3, 4)] 0 :: Int =-> Int
-- >>> f # 1
-- 2
-- >>> f # 2
-- 0
-- >>> f # 3
-- 4
data (=->) a b = TabularFun {funcTable :: [(a, b)], defaultFuncValue :: b}
  deriving (Show, Eq, Generic, Generic1, Lift, NFData, NFData1, Serial)

instance (Serial a, Serial b) => Cereal.Serialize (a =-> b) where
  put = serialize
  get = deserialize

instance (Serial a, Serial b) => Binary.Binary (a =-> b) where
  put = serialize
  get = deserialize

infixr 0 =->

instance (Eq a) => Function (a =-> b) a b where
  (TabularFun table d) # a = go table
    where
      go [] = d
      go ((av, bv) : s)
        | a == av = bv
        | otherwise = go s

instance (Hashable a, Hashable b) => Hashable (a =-> b)

instance
  (SupportedNonFuncPrim a, SupportedPrim b) =>
  SupportedPrimConstraint (a =-> b)
  where
  type
    PrimConstraint (a =-> b) =
      ( SupportedNonFuncPrim a,
        SupportedPrim b,
        NonFuncPrimConstraint a,
        PrimConstraint b
      )

instance (SupportedNonFuncPrim a, SupportedPrim b) => SBVRep (a =-> b) where
  type SBVType (a =-> b) = SBV.SBV (NonFuncSBVBaseType a) -> SBVType b

instance
  (SupportedPrim a, SupportedPrim b, Eq a, SupportedPrim (a =-> b)) =>
  PEvalApplyTerm (a =-> b) a b
  where
  pevalApplyTerm = totalize2 doPevalApplyTerm applyTerm
    where
      doPevalApplyTerm ::
        (SupportedPrim a, SupportedPrim b) =>
        Term (a =-> b) ->
        Term a ->
        Maybe (Term b)
      doPevalApplyTerm (ConTerm f) (ConTerm a) = Just $ conTerm $ f # a
      doPevalApplyTerm (ConTerm (TabularFun f d)) a = Just $ go f
        where
          go [] = conTerm d
          go ((x, y) : xs) =
            pevalITETerm (pevalEqTerm a (conTerm x)) (conTerm y) (go xs)
      doPevalApplyTerm (ITETerm cond t f) v =
        Just $ pevalITETerm cond (pevalApplyTerm t v) (pevalApplyTerm f v)
      doPevalApplyTerm _ _ = Nothing
  sbvApplyTerm f a =
    withPrim @(a =-> b) $ withNonFuncPrim @a $ f a

instance (Apply t, Eq a) => Apply (a =-> t) where
  type FunType (a =-> t) = a -> FunType t
  apply uf a = apply (uf # a)

lowerTFunCon ::
  forall a b.
  ( SupportedNonFuncPrim a,
    SupportedPrim b,
    SBV.Mergeable (SBVType b)
  ) =>
  (a =-> b) ->
  ( SBV.SBV (NonFuncSBVBaseType a) ->
    SBVType b
  )
lowerTFunCon (TabularFun l d) = withNonFuncPrim @a $ go l d
  where
    go [] d _ = conSBVTerm d
    go ((x, r) : xs) d v =
      SBV.ite (conNonFuncSBVTerm x SBV..== v) (conSBVTerm r) (go xs d v)

parseTabularFunSMTModelResult ::
  forall a b.
  (SupportedNonFuncPrim a, SupportedPrim b) =>
  Int ->
  ([([SBVD.CV], SBVD.CV)], SBVD.CV) ->
  a =-> b
parseTabularFunSMTModelResult level (l, s) =
  TabularFun
    ( second
        ( \r ->
            case r of
              [([], v)] -> parseSMTModelResult (level + 1) ([], v)
              _ -> parseSMTModelResult (level + 1) (r, s)
        )
        <$> partitionCVArg @a l
    )
    (parseSMTModelResult (level + 1) ([], s))

supportedPrimFunUpTo
  [|TabularFun [] defaultValue|]
  [|pevalITEBasicTerm|]
  [|parseTabularFunSMTModelResult|]
  ( \tyVars ->
      [|
        \f ->
          withNonFuncPrim @($(last tyVars)) $
            lowerTFunCon f
        |]
  )
  "TabularFun"
  "tfunc"
  ''(=->)
  8
