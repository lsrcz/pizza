{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.Prim.Internal.Serialize
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.Prim.Internal.Serialize () where

import Control.Monad (replicateM, unless, when)
import Control.Monad.State (StateT, evalStateT)
import qualified Control.Monad.State as State
import qualified Data.Binary as Binary
import Data.Bytes.Get (MonadGet (getWord8))
import Data.Bytes.Put (MonadPut (putWord8))
import Data.Bytes.Serial (Serial (deserialize, serialize))
import Data.Foldable (traverse_)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Hashable (Hashable (hashWithSalt))
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Proxy (Proxy (Proxy))
import qualified Data.Serialize as Cereal
import Data.Word (Word8)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import GHC.Stack (HasCallStack)
import GHC.TypeNats (KnownNat, natVal, type (+), type (<=))
import Grisette.Internal.SymPrim.AlgReal (AlgReal)
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.FP
  ( FP,
    FPRoundingMode,
    ValidFP,
    checkDynamicValidFP,
    invalidFPMessage,
    withUnsafeValidFP,
  )
import Grisette.Internal.SymPrim.GeneralFun (type (-->) (GeneralFun))
import Grisette.Internal.SymPrim.Prim.Internal.Caches (Id)
import Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalBitCastTerm ()
import Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalDivModIntegralTerm ()
import Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalFloatingTerm ()
import Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalFractionalTerm ()
import Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalFromIntegralTerm ()
import Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalIEEEFPConvertibleTerm ()
import Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalNumTerm ()
import Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalOrdTerm ()
import Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalRotateTerm ()
import Grisette.Internal.SymPrim.Prim.Internal.Instances.PEvalShiftTerm ()
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( FPBinaryOp,
    FPRoundingBinaryOp,
    FPRoundingUnaryOp,
    FPTrait,
    FPUnaryOp,
    IsSymbolKind (decideSymbolKind),
    ModelValue (ModelValue),
    PEvalBitCastTerm,
    PEvalBitwiseTerm,
    PEvalDivModIntegralTerm,
    PEvalFloatingTerm,
    PEvalFractionalTerm,
    PEvalFromIntegralTerm,
    PEvalIEEEFPConvertibleTerm,
    PEvalNumTerm,
    PEvalOrdTerm,
    PEvalRotateTerm,
    PEvalShiftTerm,
    SomeTypedAnySymbol,
    SomeTypedSymbol (SomeTypedSymbol),
    SupportedNonFuncPrim,
    SupportedPrim (primTypeRep, withPrim),
    Term,
    TypedAnySymbol,
    TypedConstantSymbol,
    TypedSymbol (TypedSymbol),
    absNumTerm,
    addNumTerm,
    andBitsTerm,
    andTerm,
    applyTerm,
    bitCastOrTerm,
    bitCastTerm,
    bvConcatTerm,
    bvExtendTerm,
    bvSelectTerm,
    complementBitsTerm,
    conTerm,
    distinctTerm,
    divIntegralTerm,
    eqTerm,
    existsTerm,
    fdivTerm,
    floatingUnaryTerm,
    forallTerm,
    fpBinaryTerm,
    fpFMATerm,
    fpRoundingBinaryTerm,
    fpRoundingUnaryTerm,
    fpTraitTerm,
    fpUnaryTerm,
    fromFPOrTerm,
    fromIntegralTerm,
    iteTerm,
    leOrdTerm,
    ltOrdTerm,
    modIntegralTerm,
    mulNumTerm,
    negNumTerm,
    notTerm,
    orBitsTerm,
    orTerm,
    powerTerm,
    quotIntegralTerm,
    recipTerm,
    remIntegralTerm,
    rotateLeftTerm,
    rotateRightTerm,
    shiftLeftTerm,
    shiftRightTerm,
    signumNumTerm,
    someTypedSymbol,
    symTerm,
    termId,
    toFPTerm,
    xorBitsTerm,
    pattern AbsNumTerm,
    pattern AddNumTerm,
    pattern AndBitsTerm,
    pattern AndTerm,
    pattern ApplyTerm,
    pattern BVConcatTerm,
    pattern BVExtendTerm,
    pattern BVSelectTerm,
    pattern BitCastOrTerm,
    pattern BitCastTerm,
    pattern ComplementBitsTerm,
    pattern ConTerm,
    pattern DistinctTerm,
    pattern DivIntegralTerm,
    pattern EqTerm,
    pattern ExistsTerm,
    pattern FPBinaryTerm,
    pattern FPFMATerm,
    pattern FPRoundingBinaryTerm,
    pattern FPRoundingUnaryTerm,
    pattern FPTraitTerm,
    pattern FPUnaryTerm,
    pattern FdivTerm,
    pattern FloatingUnaryTerm,
    pattern ForallTerm,
    pattern FromFPOrTerm,
    pattern FromIntegralTerm,
    pattern ITETerm,
    pattern LeOrdTerm,
    pattern LtOrdTerm,
    pattern ModIntegralTerm,
    pattern MulNumTerm,
    pattern NegNumTerm,
    pattern NotTerm,
    pattern OrBitsTerm,
    pattern OrTerm,
    pattern PowerTerm,
    pattern QuotIntegralTerm,
    pattern RecipTerm,
    pattern RemIntegralTerm,
    pattern RotateLeftTerm,
    pattern RotateRightTerm,
    pattern ShiftLeftTerm,
    pattern ShiftRightTerm,
    pattern SignumNumTerm,
    pattern SupportedTerm,
    pattern SymTerm,
    pattern ToFPTerm,
    pattern XorBitsTerm,
  )
import Grisette.Internal.SymPrim.Prim.SomeTerm
  ( SomeTerm (SomeTerm),
    someTerm,
  )
import Grisette.Internal.SymPrim.Prim.TermUtils (castTerm)
import Grisette.Internal.SymPrim.TabularFun (type (=->))
import Grisette.Internal.Utils.Parameterized
  ( KnownProof (KnownProof),
    LeqProof (LeqProof),
    NatRepr,
    SomeNatRepr (SomeNatRepr),
    SomePositiveNatRepr (SomePositiveNatRepr),
    mkNatRepr,
    mkPositiveNatRepr,
    unsafeAxiom,
    unsafeKnownProof,
    unsafeLeqProof,
  )
import Type.Reflection
  ( SomeTypeRep (SomeTypeRep),
    TypeRep,
    Typeable,
    eqTypeRep,
    someTypeRep,
    typeRep,
    pattern App,
    pattern Con,
    type (:~:) (Refl),
    type (:~~:) (HRefl),
  )

data KnownNonFuncType where
  BoolType :: KnownNonFuncType
  IntegerType :: KnownNonFuncType
  WordNType :: (KnownNat n, 1 <= n) => Proxy n -> KnownNonFuncType
  IntNType :: (KnownNat n, 1 <= n) => Proxy n -> KnownNonFuncType
  FPType :: (ValidFP eb sb) => Proxy eb -> Proxy sb -> KnownNonFuncType
  FPRoundingModeType :: KnownNonFuncType
  AlgRealType :: KnownNonFuncType

instance Eq KnownNonFuncType where
  BoolType == BoolType = True
  IntegerType == IntegerType = True
  WordNType p == WordNType q = natVal p == natVal q
  IntNType p == IntNType q = natVal p == natVal q
  FPType p q == FPType r s = natVal p == natVal r && natVal q == natVal s
  FPRoundingModeType == FPRoundingModeType = True
  AlgRealType == AlgRealType = True
  _ == _ = False

instance Hashable KnownNonFuncType where
  hashWithSalt s BoolType = s `hashWithSalt` (0 :: Int)
  hashWithSalt s IntegerType = s `hashWithSalt` (1 :: Int)
  hashWithSalt s (WordNType p) =
    s `hashWithSalt` (2 :: Int) `hashWithSalt` natVal p
  hashWithSalt s (IntNType p) =
    s `hashWithSalt` (3 :: Int) `hashWithSalt` natVal p
  hashWithSalt s (FPType p q) =
    s `hashWithSalt` (4 :: Int) `hashWithSalt` natVal p `hashWithSalt` natVal q
  hashWithSalt s FPRoundingModeType = s `hashWithSalt` (5 :: Int)
  hashWithSalt s AlgRealType = s `hashWithSalt` (6 :: Int)

data KnownNonFuncTypeWitness where
  KnownNonFuncTypeWitness ::
    ( SupportedNonFuncPrim a,
      Eq a,
      Show a,
      Hashable a,
      Typeable a,
      Serial a
    ) =>
    Proxy a ->
    KnownNonFuncTypeWitness

witnessKnownNonFuncType :: KnownNonFuncType -> KnownNonFuncTypeWitness
witnessKnownNonFuncType BoolType = KnownNonFuncTypeWitness (Proxy @Bool)
witnessKnownNonFuncType IntegerType = KnownNonFuncTypeWitness (Proxy @Integer)
witnessKnownNonFuncType (WordNType (Proxy :: Proxy n)) =
  KnownNonFuncTypeWitness (Proxy @(WordN n))
witnessKnownNonFuncType (IntNType (Proxy :: Proxy n)) =
  KnownNonFuncTypeWitness (Proxy @(IntN n))
witnessKnownNonFuncType (FPType (Proxy :: Proxy eb) (Proxy :: Proxy sb)) =
  KnownNonFuncTypeWitness (Proxy @(FP eb sb))
witnessKnownNonFuncType FPRoundingModeType =
  KnownNonFuncTypeWitness (Proxy @FPRoundingMode)
witnessKnownNonFuncType AlgRealType = KnownNonFuncTypeWitness (Proxy @AlgReal)

data KnownType where
  NonFuncType :: KnownNonFuncType -> KnownType
  TabularFunType :: [KnownNonFuncType] -> KnownType
  GeneralFunType :: [KnownNonFuncType] -> KnownType
  deriving (Eq, Generic, Hashable)

data KnownTypeWitness where
  KnownTypeWitness ::
    ( SupportedPrim a,
      Eq a,
      Show a,
      Hashable a,
      Typeable a,
      Serial a
    ) =>
    Proxy a ->
    KnownTypeWitness

witnessKnownType :: KnownType -> KnownTypeWitness
witnessKnownType (NonFuncType nf) = case witnessKnownNonFuncType nf of
  KnownNonFuncTypeWitness (Proxy :: Proxy a) -> KnownTypeWitness (Proxy @a)
witnessKnownType (TabularFunType [a, b]) =
  case (witnessKnownNonFuncType a, witnessKnownNonFuncType b) of
    ( KnownNonFuncTypeWitness (Proxy :: Proxy a),
      KnownNonFuncTypeWitness (Proxy :: Proxy b)
      ) -> KnownTypeWitness (Proxy @(a =-> b))
witnessKnownType (TabularFunType [a, b, c]) =
  case ( witnessKnownNonFuncType a,
         witnessKnownNonFuncType b,
         witnessKnownNonFuncType c
       ) of
    ( KnownNonFuncTypeWitness (Proxy :: Proxy a),
      KnownNonFuncTypeWitness (Proxy :: Proxy b),
      KnownNonFuncTypeWitness (Proxy :: Proxy c)
      ) -> KnownTypeWitness (Proxy @(a =-> b =-> c))
witnessKnownType (TabularFunType [a, b, c, d]) =
  case ( witnessKnownNonFuncType a,
         witnessKnownNonFuncType b,
         witnessKnownNonFuncType c,
         witnessKnownNonFuncType d
       ) of
    ( KnownNonFuncTypeWitness (Proxy :: Proxy a),
      KnownNonFuncTypeWitness (Proxy :: Proxy b),
      KnownNonFuncTypeWitness (Proxy :: Proxy c),
      KnownNonFuncTypeWitness (Proxy :: Proxy d)
      ) -> KnownTypeWitness (Proxy @(a =-> b =-> c =-> d))
witnessKnownType (TabularFunType [a, b, c, d, e]) =
  case ( witnessKnownNonFuncType a,
         witnessKnownNonFuncType b,
         witnessKnownNonFuncType c,
         witnessKnownNonFuncType d,
         witnessKnownNonFuncType e
       ) of
    ( KnownNonFuncTypeWitness (Proxy :: Proxy a),
      KnownNonFuncTypeWitness (Proxy :: Proxy b),
      KnownNonFuncTypeWitness (Proxy :: Proxy c),
      KnownNonFuncTypeWitness (Proxy :: Proxy d),
      KnownNonFuncTypeWitness (Proxy :: Proxy e)
      ) -> KnownTypeWitness (Proxy @(a =-> b =-> c =-> d =-> e))
witnessKnownType (TabularFunType [a, b, c, d, e, f]) =
  case ( witnessKnownNonFuncType a,
         witnessKnownNonFuncType b,
         witnessKnownNonFuncType c,
         witnessKnownNonFuncType d,
         witnessKnownNonFuncType e,
         witnessKnownNonFuncType f
       ) of
    ( KnownNonFuncTypeWitness (Proxy :: Proxy a),
      KnownNonFuncTypeWitness (Proxy :: Proxy b),
      KnownNonFuncTypeWitness (Proxy :: Proxy c),
      KnownNonFuncTypeWitness (Proxy :: Proxy d),
      KnownNonFuncTypeWitness (Proxy :: Proxy e),
      KnownNonFuncTypeWitness (Proxy :: Proxy f)
      ) -> KnownTypeWitness (Proxy @(a =-> b =-> c =-> d =-> e =-> f))
witnessKnownType (TabularFunType [a, b, c, d, e, f, g]) =
  case ( witnessKnownNonFuncType a,
         witnessKnownNonFuncType b,
         witnessKnownNonFuncType c,
         witnessKnownNonFuncType d,
         witnessKnownNonFuncType e,
         witnessKnownNonFuncType f,
         witnessKnownNonFuncType g
       ) of
    ( KnownNonFuncTypeWitness (Proxy :: Proxy a),
      KnownNonFuncTypeWitness (Proxy :: Proxy b),
      KnownNonFuncTypeWitness (Proxy :: Proxy c),
      KnownNonFuncTypeWitness (Proxy :: Proxy d),
      KnownNonFuncTypeWitness (Proxy :: Proxy e),
      KnownNonFuncTypeWitness (Proxy :: Proxy f),
      KnownNonFuncTypeWitness (Proxy :: Proxy g)
      ) -> KnownTypeWitness (Proxy @(a =-> b =-> c =-> d =-> e =-> f =-> g))
witnessKnownType (TabularFunType [a, b, c, d, e, f, g, h]) =
  case ( witnessKnownNonFuncType a,
         witnessKnownNonFuncType b,
         witnessKnownNonFuncType c,
         witnessKnownNonFuncType d,
         witnessKnownNonFuncType e,
         witnessKnownNonFuncType f,
         witnessKnownNonFuncType g,
         witnessKnownNonFuncType h
       ) of
    ( KnownNonFuncTypeWitness (Proxy :: Proxy a),
      KnownNonFuncTypeWitness (Proxy :: Proxy b),
      KnownNonFuncTypeWitness (Proxy :: Proxy c),
      KnownNonFuncTypeWitness (Proxy :: Proxy d),
      KnownNonFuncTypeWitness (Proxy :: Proxy e),
      KnownNonFuncTypeWitness (Proxy :: Proxy f),
      KnownNonFuncTypeWitness (Proxy :: Proxy g),
      KnownNonFuncTypeWitness (Proxy :: Proxy h)
      ) ->
        KnownTypeWitness (Proxy @(a =-> b =-> c =-> d =-> e =-> f =-> g =-> h))
witnessKnownType (GeneralFunType [a, b]) =
  case (witnessKnownNonFuncType a, witnessKnownNonFuncType b) of
    ( KnownNonFuncTypeWitness (Proxy :: Proxy a),
      KnownNonFuncTypeWitness (Proxy :: Proxy b)
      ) -> KnownTypeWitness (Proxy @(a --> b))
witnessKnownType (GeneralFunType [a, b, c]) =
  case ( witnessKnownNonFuncType a,
         witnessKnownNonFuncType b,
         witnessKnownNonFuncType c
       ) of
    ( KnownNonFuncTypeWitness (Proxy :: Proxy a),
      KnownNonFuncTypeWitness (Proxy :: Proxy b),
      KnownNonFuncTypeWitness (Proxy :: Proxy c)
      ) -> KnownTypeWitness (Proxy @(a --> b --> c))
witnessKnownType (GeneralFunType [a, b, c, d]) =
  case ( witnessKnownNonFuncType a,
         witnessKnownNonFuncType b,
         witnessKnownNonFuncType c,
         witnessKnownNonFuncType d
       ) of
    ( KnownNonFuncTypeWitness (Proxy :: Proxy a),
      KnownNonFuncTypeWitness (Proxy :: Proxy b),
      KnownNonFuncTypeWitness (Proxy :: Proxy c),
      KnownNonFuncTypeWitness (Proxy :: Proxy d)
      ) -> KnownTypeWitness (Proxy @(a --> b --> c --> d))
witnessKnownType (GeneralFunType [a, b, c, d, e]) =
  case ( witnessKnownNonFuncType a,
         witnessKnownNonFuncType b,
         witnessKnownNonFuncType c,
         witnessKnownNonFuncType d,
         witnessKnownNonFuncType e
       ) of
    ( KnownNonFuncTypeWitness (Proxy :: Proxy a),
      KnownNonFuncTypeWitness (Proxy :: Proxy b),
      KnownNonFuncTypeWitness (Proxy :: Proxy c),
      KnownNonFuncTypeWitness (Proxy :: Proxy d),
      KnownNonFuncTypeWitness (Proxy :: Proxy e)
      ) -> KnownTypeWitness (Proxy @(a --> b --> c --> d --> e))
witnessKnownType (GeneralFunType [a, b, c, d, e, f]) =
  case ( witnessKnownNonFuncType a,
         witnessKnownNonFuncType b,
         witnessKnownNonFuncType c,
         witnessKnownNonFuncType d,
         witnessKnownNonFuncType e,
         witnessKnownNonFuncType f
       ) of
    ( KnownNonFuncTypeWitness (Proxy :: Proxy a),
      KnownNonFuncTypeWitness (Proxy :: Proxy b),
      KnownNonFuncTypeWitness (Proxy :: Proxy c),
      KnownNonFuncTypeWitness (Proxy :: Proxy d),
      KnownNonFuncTypeWitness (Proxy :: Proxy e),
      KnownNonFuncTypeWitness (Proxy :: Proxy f)
      ) -> KnownTypeWitness (Proxy @(a --> b --> c --> d --> e --> f))
witnessKnownType (GeneralFunType [a, b, c, d, e, f, g]) =
  case ( witnessKnownNonFuncType a,
         witnessKnownNonFuncType b,
         witnessKnownNonFuncType c,
         witnessKnownNonFuncType d,
         witnessKnownNonFuncType e,
         witnessKnownNonFuncType f,
         witnessKnownNonFuncType g
       ) of
    ( KnownNonFuncTypeWitness (Proxy :: Proxy a),
      KnownNonFuncTypeWitness (Proxy :: Proxy b),
      KnownNonFuncTypeWitness (Proxy :: Proxy c),
      KnownNonFuncTypeWitness (Proxy :: Proxy d),
      KnownNonFuncTypeWitness (Proxy :: Proxy e),
      KnownNonFuncTypeWitness (Proxy :: Proxy f),
      KnownNonFuncTypeWitness (Proxy :: Proxy g)
      ) -> KnownTypeWitness (Proxy @(a --> b --> c --> d --> e --> f --> g))
witnessKnownType (GeneralFunType [a, b, c, d, e, f, g, h]) =
  case ( witnessKnownNonFuncType a,
         witnessKnownNonFuncType b,
         witnessKnownNonFuncType c,
         witnessKnownNonFuncType d,
         witnessKnownNonFuncType e,
         witnessKnownNonFuncType f,
         witnessKnownNonFuncType g,
         witnessKnownNonFuncType h
       ) of
    ( KnownNonFuncTypeWitness (Proxy :: Proxy a),
      KnownNonFuncTypeWitness (Proxy :: Proxy b),
      KnownNonFuncTypeWitness (Proxy :: Proxy c),
      KnownNonFuncTypeWitness (Proxy :: Proxy d),
      KnownNonFuncTypeWitness (Proxy :: Proxy e),
      KnownNonFuncTypeWitness (Proxy :: Proxy f),
      KnownNonFuncTypeWitness (Proxy :: Proxy g),
      KnownNonFuncTypeWitness (Proxy :: Proxy h)
      ) ->
        KnownTypeWitness (Proxy @(a --> b --> c --> d --> e --> f --> g --> h))
witnessKnownType l = error $ "witnessKnownType: unsupported type: " <> show l

instance Show KnownNonFuncType where
  show BoolType = "Bool"
  show IntegerType = "Integer"
  show (WordNType (_ :: p n)) = "WordN " <> show (natVal (Proxy @n))
  show (IntNType (_ :: p n)) = "IntN " <> show (natVal (Proxy @n))
  show (FPType (_ :: p eb) (_ :: q sb)) =
    "FP "
      <> show (natVal (Proxy @eb))
      <> " "
      <> show (natVal (Proxy @sb))
  show FPRoundingModeType = "FPRoundingMode"
  show AlgRealType = "AlgReal"

instance Show KnownType where
  show (NonFuncType t) = show t
  show (TabularFunType ts) = intercalate " =-> " $ show <$> ts
  show (GeneralFunType ts) = intercalate " --> " $ show <$> ts

knownNonFuncType ::
  forall a p. (SupportedNonFuncPrim a) => p a -> KnownNonFuncType
knownNonFuncType _ =
  case tr of
    _ | SomeTypeRep tr == someTypeRep (Proxy @Bool) -> BoolType
    _ | SomeTypeRep tr == someTypeRep (Proxy @Integer) -> IntegerType
    _
      | SomeTypeRep tr == someTypeRep (Proxy @FPRoundingMode) ->
          FPRoundingModeType
    _ | SomeTypeRep tr == someTypeRep (Proxy @AlgReal) -> AlgRealType
    App (ta@(Con _) :: TypeRep w) (_ :: TypeRep n) ->
      case ( eqTypeRep ta (typeRep @WordN),
             eqTypeRep ta (typeRep @IntN)
           ) of
        (Just HRefl, _) -> withPrim @a $ WordNType (Proxy @n)
        (_, Just HRefl) -> withPrim @a $ IntNType (Proxy @n)
        _ -> err
    App (App (tf :: TypeRep f) (_ :: TypeRep a0)) (_ :: TypeRep a1) ->
      case eqTypeRep tf (typeRep @FP) of
        Just HRefl -> withPrim @a $ FPType (Proxy @a0) (Proxy @a1)
        _ -> err
    _ -> err
  where
    tr = primTypeRep @a
    err = error $ "knownNonFuncType: unsupported type: " <> show tr

knownType ::
  forall a p. (SupportedPrim a) => p a -> KnownType
knownType _ =
  case tr of
    _ | SomeTypeRep tr == someTypeRep (Proxy @Bool) -> NonFuncType BoolType
    _
      | SomeTypeRep tr == someTypeRep (Proxy @Integer) ->
          NonFuncType IntegerType
    _
      | SomeTypeRep tr == someTypeRep (Proxy @FPRoundingMode) ->
          NonFuncType FPRoundingModeType
    _
      | SomeTypeRep tr == someTypeRep (Proxy @AlgReal) ->
          NonFuncType AlgRealType
    App (ta@(Con _) :: TypeRep w) (_ :: TypeRep n) ->
      case ( eqTypeRep ta (typeRep @WordN),
             eqTypeRep ta (typeRep @IntN)
           ) of
        (Just HRefl, _) -> withPrim @a $ NonFuncType $ WordNType (Proxy @n)
        (_, Just HRefl) -> withPrim @a $ NonFuncType $ IntNType (Proxy @n)
        _ -> err
    App (App (tf :: TypeRep f) (_ :: TypeRep a0)) (_ :: TypeRep a1) ->
      case ( eqTypeRep tf (typeRep @FP),
             eqTypeRep tf (typeRep @(=->)),
             eqTypeRep tf (typeRep @(-->))
           ) of
        (Just HRefl, _, _) ->
          withPrim @a $ NonFuncType $ FPType (Proxy @a0) (Proxy @a1)
        (_, Just HRefl, _) ->
          withPrim @a $
            let arg = knownType (Proxy @a0)
                ret = knownType (Proxy @a1)
             in case arg of
                  NonFuncType n -> case ret of
                    NonFuncType m -> TabularFunType [n, m]
                    TabularFunType ns -> TabularFunType (n : ns)
                    _ -> err
                  _ -> err
        (_, _, Just HRefl) ->
          withPrim @a $
            let arg = knownType (Proxy @a0)
                ret = knownType (Proxy @a1)
             in case arg of
                  NonFuncType n -> case ret of
                    NonFuncType m -> GeneralFunType [n, m]
                    GeneralFunType ns -> GeneralFunType (n : ns)
                    _ -> err
                  _ -> err
        _ -> err
    _ -> err
  where
    tr = primTypeRep @a
    err = error $ "knownType: unsupported type: " <> show tr

-- Bool: 0
-- Integer: 1
-- WordN: 2
-- IntN: 3
-- FP: 4
-- FPRoundingMode: 5
-- AlgReal: 6
serializeKnownNonFuncType :: (MonadPut m) => KnownNonFuncType -> m ()
serializeKnownNonFuncType BoolType = putWord8 0
serializeKnownNonFuncType IntegerType = putWord8 1
serializeKnownNonFuncType (WordNType (Proxy :: Proxy n)) =
  putWord8 2 >> serialize (natVal (Proxy @n))
serializeKnownNonFuncType (IntNType (Proxy :: Proxy n)) =
  putWord8 3 >> serialize (natVal (Proxy @n))
serializeKnownNonFuncType (FPType (Proxy :: Proxy eb) (Proxy :: Proxy sb)) =
  putWord8 4 >> serialize (natVal (Proxy @eb)) >> serialize (natVal (Proxy @sb))
serializeKnownNonFuncType FPRoundingModeType = putWord8 5
serializeKnownNonFuncType AlgRealType = putWord8 6

serializeKnownType :: (MonadPut m) => KnownType -> m ()
serializeKnownType (NonFuncType t) = putWord8 0 >> serializeKnownNonFuncType t
serializeKnownType (TabularFunType ts) =
  putWord8 1
    >> putWord8 (fromIntegral $ length ts)
    >> traverse_ serializeKnownNonFuncType ts
serializeKnownType (GeneralFunType ts) =
  putWord8 2
    >> putWord8 (fromIntegral $ length ts)
    >> traverse_ serializeKnownNonFuncType ts

deserializeKnownNonFuncType :: (MonadGet m) => m KnownNonFuncType
deserializeKnownNonFuncType = do
  tag <- getWord8
  case tag of
    0 -> return BoolType
    1 -> return IntegerType
    2 -> do
      n <- deserialize @Natural
      when (n == 0) $ fail "deserializeKnownNonFuncType: WordN 0 is not allowed"
      case mkPositiveNatRepr n of
        SomePositiveNatRepr (_ :: NatRepr n) -> return $ WordNType (Proxy @n)
    3 -> do
      n <- deserialize @Natural
      when (n == 0) $ fail "deserializeKnownNonFuncType: IntN 0 is not allowed"
      case mkPositiveNatRepr n of
        SomePositiveNatRepr (_ :: NatRepr n) -> return $ IntNType (Proxy @n)
    4 -> do
      eb <- deserialize @Natural
      sb <- deserialize @Natural
      unless (checkDynamicValidFP eb sb) $ fail invalidFPMessage
      case (mkPositiveNatRepr eb, mkPositiveNatRepr sb) of
        ( SomePositiveNatRepr (_ :: NatRepr eb),
          SomePositiveNatRepr (_ :: NatRepr sb)
          ) ->
            withUnsafeValidFP @eb @sb $ return $ FPType (Proxy @eb) (Proxy @sb)
    5 -> return FPRoundingModeType
    6 -> return AlgRealType
    _ -> fail "deserializeKnownNonFuncType: Unknown type tag"

deserializeKnownType :: (MonadGet m) => m KnownType
deserializeKnownType = do
  tag <- getWord8
  case tag of
    0 -> NonFuncType <$> deserializeKnownNonFuncType
    1 -> do
      n <- getWord8
      nfs <- replicateM (fromIntegral n) deserializeKnownNonFuncType
      return $ TabularFunType nfs
    2 -> do
      n <- getWord8
      nfs <- replicateM (fromIntegral n) deserializeKnownNonFuncType
      return $ GeneralFunType nfs
    _ -> fail "deserializeKnownType: Unknown type tag"

instance Serial KnownType where
  serialize = serializeKnownType
  deserialize = deserializeKnownType

instance Cereal.Serialize KnownType where
  put = serialize
  get = deserialize

instance Binary.Binary KnownType where
  put = serialize
  get = deserialize

instance Serial KnownNonFuncType where
  serialize = serializeKnownNonFuncType
  deserialize = deserializeKnownNonFuncType

instance Cereal.Serialize KnownNonFuncType where
  put = serialize
  get = deserialize

instance Binary.Binary KnownNonFuncType where
  put = serialize
  get = deserialize

instance (IsSymbolKind knd) => Serial (SomeTypedSymbol knd) where
  serialize (SomeTypedSymbol tsb@(TypedSymbol sb)) =
    case decideSymbolKind @knd of
      Left HRefl -> do
        serializeKnownNonFuncType $ knownNonFuncType tsb
        serialize sb
      Right HRefl -> do
        serializeKnownType $ knownType tsb
        serialize sb
  deserialize = case decideSymbolKind @knd of
    Left HRefl -> do
      kt <- deserializeKnownNonFuncType
      case witnessKnownNonFuncType kt of
        KnownNonFuncTypeWitness (Proxy :: Proxy a) -> do
          sb <- deserialize
          return $ SomeTypedSymbol $ TypedSymbol @a sb
    Right HRefl -> do
      kt <- deserializeKnownType
      case witnessKnownType kt of
        KnownTypeWitness (Proxy :: Proxy a) -> do
          sb <- deserialize
          return $ SomeTypedSymbol $ TypedSymbol @a sb

instance (IsSymbolKind knd) => Cereal.Serialize (SomeTypedSymbol knd) where
  put = serialize
  get = deserialize

instance (IsSymbolKind knd) => Binary.Binary (SomeTypedSymbol knd) where
  put = serialize
  get = deserialize

instance (IsSymbolKind knd, Typeable a) => Serial (TypedSymbol knd a) where
  serialize tsb = serialize $ someTypedSymbol tsb
  deserialize = do
    SomeTypedSymbol (tsb@TypedSymbol {} :: TypedSymbol knd b) <- deserialize
    case eqTypeRep (typeRep @a) (primTypeRep @b) of
      Just HRefl -> return tsb
      Nothing -> fail "deserialize TypedSymbol: type mismatch"

instance
  (IsSymbolKind knd, Typeable a) =>
  Cereal.Serialize (TypedSymbol knd a)
  where
  put = serialize
  get = deserialize

instance
  (IsSymbolKind knd, Typeable a) =>
  Binary.Binary (TypedSymbol knd a)
  where
  put = serialize
  get = deserialize

conTermTag :: Word8
conTermTag = 0

symTermTag :: Word8
symTermTag = 1

forallTermTag :: Word8
forallTermTag = 2

existsTermTag :: Word8
existsTermTag = 3

notTermTag :: Word8
notTermTag = 4

orTermTag :: Word8
orTermTag = 5

andTermTag :: Word8
andTermTag = 6

eqTermTag :: Word8
eqTermTag = 7

distinctTermTag :: Word8
distinctTermTag = 8

iteTermTag :: Word8
iteTermTag = 9

addNumTermTag :: Word8
addNumTermTag = 10

negNumTermTag :: Word8
negNumTermTag = 11

mulNumTermTag :: Word8
mulNumTermTag = 12

absNumTermTag :: Word8
absNumTermTag = 13

signumNumTermTag :: Word8
signumNumTermTag = 14

ltOrdTermTag :: Word8
ltOrdTermTag = 15

leOrdTermTag :: Word8
leOrdTermTag = 16

andBitsTermTag :: Word8
andBitsTermTag = 17

orBitsTermTag :: Word8
orBitsTermTag = 18

xorBitsTermTag :: Word8
xorBitsTermTag = 19

complementBitsTermTag :: Word8
complementBitsTermTag = 20

shiftLeftTermTag :: Word8
shiftLeftTermTag = 21

shiftRightTermTag :: Word8
shiftRightTermTag = 22

rotateLeftTermTag :: Word8
rotateLeftTermTag = 23

rotateRightTermTag :: Word8
rotateRightTermTag = 24

bitCastTermTag :: Word8
bitCastTermTag = 25

bitCastOrTermTag :: Word8
bitCastOrTermTag = 26

bvConcatTermTag :: Word8
bvConcatTermTag = 27

bvSelectTermTag :: Word8
bvSelectTermTag = 28

bvExtendTermTag :: Word8
bvExtendTermTag = 29

applyTermTag :: Word8
applyTermTag = 30

divIntegralTermTag :: Word8
divIntegralTermTag = 31

modIntegralTermTag :: Word8
modIntegralTermTag = 32

quotIntegralTermTag :: Word8
quotIntegralTermTag = 33

remIntegralTermTag :: Word8
remIntegralTermTag = 34

fpTraitTermTag :: Word8
fpTraitTermTag = 35

fdivTermTag :: Word8
fdivTermTag = 36

recipTermTag :: Word8
recipTermTag = 37

floatingUnaryTermTag :: Word8
floatingUnaryTermTag = 38

powerTermTag :: Word8
powerTermTag = 39

fpUnaryTermTag :: Word8
fpUnaryTermTag = 40

fpBinaryTermTag :: Word8
fpBinaryTermTag = 41

fpRoundingUnaryTermTag :: Word8
fpRoundingUnaryTermTag = 42

fpRoundingBinaryTermTag :: Word8
fpRoundingBinaryTermTag = 43

fpFMATermTag :: Word8
fpFMATermTag = 44

fromIntegralTermTag :: Word8
fromIntegralTermTag = 45

fromFPOrTermTag :: Word8
fromFPOrTermTag = 46

toFPTermTag :: Word8
toFPTermTag = 47

terminalTag :: Word8
terminalTag = 255

asBoolTerm :: (HasCallStack) => SomeTerm -> Term Bool
asBoolTerm (SomeTerm (t :: Term a)) =
  case eqTypeRep (primTypeRep @Bool) (primTypeRep @a) of
    Just HRefl -> t
    Nothing -> error "asBoolTerm: type mismatch"

asSameTypeNonEmptyTermList ::
  (HasCallStack) =>
  NonEmpty SomeTerm ->
  (forall b. NonEmpty (Term b) -> r) ->
  r
asSameTypeNonEmptyTermList (SomeTerm (t :: Term a) :| ts) f =
  f $ t :| fmap (unsafeCastTerm t) ts
  where
    unsafeCastTerm :: Term a -> SomeTerm -> Term a
    unsafeCastTerm _ (SomeTerm b) =
      case castTerm b of
        Just r -> r
        Nothing -> error "asSameTypeNonEmptyTermList: type mismatch"

asNumTypeTerm ::
  (HasCallStack) =>
  SomeTerm ->
  (forall n. (PEvalNumTerm n, PEvalIEEEFPConvertibleTerm n) => Term n -> r) ->
  r
asNumTypeTerm (SomeTerm (t1 :: Term a)) f =
  case ( eqTypeRep ta (typeRep @Integer),
         eqTypeRep ta (typeRep @AlgReal)
       ) of
    (Just HRefl, _) -> f t1
    (_, Just HRefl) -> f t1
    _ ->
      case ta of
        App (ta@(Con _) :: TypeRep w) (_ :: TypeRep n) ->
          case ( eqTypeRep ta (typeRep @WordN),
                 eqTypeRep ta (typeRep @IntN)
               ) of
            (Just HRefl, _) -> withPrim @a $ f t1
            (_, Just HRefl) -> withPrim @a $ f t1
            _ -> err
        App (App (tf :: TypeRep f) (_ :: TypeRep a0)) (_ :: TypeRep a1) ->
          case eqTypeRep tf (typeRep @FP) of
            Just HRefl ->
              withPrim @a $ withPrim @a $ f t1
            _ -> err
        _ -> err
  where
    ta = primTypeRep @a
    err = error $ "asNumTypeTerm: unsupported type: " <> show ta

asOrdTypeTerm ::
  (HasCallStack) => SomeTerm -> (forall n. (PEvalOrdTerm n) => Term n -> r) -> r
asOrdTypeTerm (SomeTerm (t1 :: Term a)) f =
  case ( eqTypeRep ta (typeRep @Integer),
         eqTypeRep ta (typeRep @AlgReal),
         eqTypeRep ta (typeRep @FPRoundingMode)
       ) of
    (Just HRefl, _, _) -> f t1
    (_, Just HRefl, _) -> f t1
    (_, _, Just HRefl) -> f t1
    _ ->
      case ta of
        App (ta@(Con _) :: TypeRep w) (_ :: TypeRep n) ->
          case ( eqTypeRep ta (typeRep @WordN),
                 eqTypeRep ta (typeRep @IntN)
               ) of
            (Just HRefl, _) -> withPrim @a $ f t1
            (_, Just HRefl) -> withPrim @a $ f t1
            _ -> err
        App (App (tf :: TypeRep f) (_ :: TypeRep a0)) (_ :: TypeRep a1) ->
          case eqTypeRep tf (typeRep @FP) of
            Just HRefl ->
              withPrim @a $ withPrim @a $ f t1
            _ -> err
        _ -> err
  where
    ta = primTypeRep @a
    err = error $ "asOrdTypeTerm: unsupported type: " <> show ta

asBitsTypeTerm ::
  (HasCallStack) =>
  SomeTerm ->
  ( forall n.
    (PEvalBitwiseTerm n, PEvalShiftTerm n, PEvalRotateTerm n) =>
    Term n ->
    r
  ) ->
  r
asBitsTypeTerm (SomeTerm (t1 :: Term a)) f =
  case ta of
    App (ta@(Con _) :: TypeRep w) (_ :: TypeRep n) ->
      case ( eqTypeRep ta (typeRep @WordN),
             eqTypeRep ta (typeRep @IntN)
           ) of
        (Just HRefl, _) -> withPrim @a $ f t1
        (_, Just HRefl) -> withPrim @a $ f t1
        _ -> err
    _ -> err
  where
    ta = primTypeRep @a
    err = error $ "asBitsTypeTerm: unsupported type: " <> show ta

asIntegralTypeTerm ::
  (HasCallStack) =>
  SomeTerm ->
  (forall n. (PEvalDivModIntegralTerm n) => Term n -> r) ->
  r
asIntegralTypeTerm (SomeTerm (t1 :: Term a)) f =
  case eqTypeRep ta (typeRep @Integer) of
    Just HRefl -> f t1
    _ -> case ta of
      App (ta@(Con _) :: TypeRep w) (_ :: TypeRep n) ->
        case (eqTypeRep ta (typeRep @WordN), eqTypeRep ta (typeRep @IntN)) of
          (Just HRefl, _) -> withPrim @a $ f t1
          (_, Just HRefl) -> withPrim @a $ f t1
          _ -> err
      _ -> err
  where
    ta = primTypeRep @a
    err = error $ "asOrdTypeTerm: unsupported type: " <> show ta

asFloatingFractionalTypeTerm ::
  (HasCallStack) =>
  SomeTerm ->
  (forall n. (PEvalFloatingTerm n, PEvalFractionalTerm n) => Term n -> r) ->
  r
asFloatingFractionalTypeTerm (SomeTerm (t1 :: Term a)) f =
  case eqTypeRep ta (typeRep @AlgReal) of
    Just HRefl -> f t1
    _ -> case ta of
      App (App (tf :: TypeRep f) (_ :: TypeRep a0)) (_ :: TypeRep a1) ->
        case eqTypeRep tf (typeRep @FP) of
          Just HRefl -> withPrim @a $ f t1
          _ -> err
      _ -> err
  where
    ta = primTypeRep @a
    err = error $ "asFloatingFractionalTypeTerm: unsupported type: " <> show ta

asFPTypeTerm ::
  (HasCallStack) =>
  SomeTerm ->
  (forall eb sb. (ValidFP eb sb) => Term (FP eb sb) -> r) ->
  r
asFPTypeTerm (SomeTerm (t1 :: Term a)) f =
  case ta of
    App (App (tf :: TypeRep f) (_ :: TypeRep a0)) (_ :: TypeRep a1) ->
      case eqTypeRep tf (typeRep @FP) of
        Just HRefl -> withPrim @a $ f t1
        _ -> err
    _ -> err
  where
    ta = primTypeRep @a
    err = error $ "asFPTypeTerm: unsupported type: " <> show ta

asSameType ::
  (HasCallStack) => Term a -> SomeTerm -> (Term a -> r) -> r
asSameType (SupportedTerm :: Term a) (SomeTerm (t2 :: Term b)) f =
  case eqTypeRep (primTypeRep @a) (primTypeRep @b) of
    Just HRefl -> f t2
    Nothing -> error "asSameType: type mismatch"

asFPRoundingTerm ::
  (HasCallStack) => SomeTerm -> (Term FPRoundingMode -> r) -> r
asFPRoundingTerm (SomeTerm (t1 :: Term a)) f =
  case eqTypeRep ta (typeRep @FPRoundingMode) of
    Just HRefl -> f t1
    _ -> err
  where
    ta = primTypeRep @a
    err = error $ "asFPRoundingTerm: unsupported type: " <> show ta

constructBitCastOrTerm :: (HasCallStack) => SomeTerm -> SomeTerm -> SomeTerm
constructBitCastOrTerm (SomeTerm (td :: Term d)) (SomeTerm (tv :: Term v)) =
  withPrim @d $
    withPrim @v $
      case (trv, trd) of
        ( App
            (App (trfp@(Con _) :: TypeRep fp) (treb :: TypeRep eb))
            (trsb :: TypeRep sb),
          App (trbv@(Con _) :: TypeRep bv) (trn :: TypeRep n)
          ) ->
            case ( eqTypeRep trfp (typeRep @FP),
                   eqTypeRep trbv (typeRep @WordN),
                   eqTypeRep trbv (typeRep @IntN)
                 ) of
              (Just HRefl, Just HRefl, _) ->
                if natVal treb + natVal trsb == natVal trn
                  then case unsafeAxiom @n @(eb + sb) of
                    Refl -> someTerm (bitCastOrTerm td tv :: Term d)
                  else err
              (Just HRefl, _, Just HRefl) ->
                if natVal treb + natVal trsb == natVal trn
                  then case unsafeAxiom @n @(eb + sb) of
                    Refl -> someTerm (bitCastOrTerm td tv :: Term d)
                  else err
              _ -> err
        _ -> err
  where
    trd = primTypeRep @d
    trv = primTypeRep @v
    err :: r
    err =
      error $
        "constructBitCastOrTerm: unsupported type: " <> show trd <> show trv

constructBitCastTerm :: (HasCallStack) => SomeTerm -> KnownType -> SomeTerm
constructBitCastTerm (SomeTerm (t1 :: Term a)) retType =
  case witnessKnownType retType of
    KnownTypeWitness (_ :: Proxy b) -> do
      let tb = primTypeRep @b
      withPrim @a $ withPrim @b $ case (eqTypeRep ta (typeRep @Bool), ta) of
        (Just HRefl, _) -> case tb of
          App (tw@(Con _) :: TypeRep w) (tn :: TypeRep n) ->
            case ( eqTypeRep tw (typeRep @WordN),
                   eqTypeRep tw (typeRep @IntN),
                   eqTypeRep tn (typeRep @1)
                 ) of
              (Just HRefl, _, Just HRefl) -> someTerm (bitCastTerm t1 :: Term b)
              (_, Just HRefl, Just HRefl) -> someTerm (bitCastTerm t1 :: Term b)
              _ -> err
          _ -> err
        (_, App (tw@(Con _) :: TypeRep w) (tn :: TypeRep n)) ->
          case (eqTypeRep tw (typeRep @WordN), eqTypeRep tw (typeRep @IntN)) of
            (Just HRefl, _) -> fromBV t1 tn tb (typeRep @IntN)
            (_, Just HRefl) -> fromBV t1 tn tb (typeRep @WordN)
            _ -> err
        _ -> err
  where
    ta = primTypeRep @a
    err :: r
    err =
      error $
        "constructBitCastTerm: unsupported type: "
          <> show ta
          <> show retType
    fromBV ::
      forall bv n b bv2.
      ( forall n. (KnownNat n, 1 <= n) => PEvalBitCastTerm (bv n) (bv2 n),
        PEvalBitCastTerm (bv 1) Bool,
        forall n eb sb.
        (KnownNat n, 1 <= n, ValidFP eb sb, (eb + sb) ~ n) =>
        PEvalBitCastTerm (bv n) (FP eb sb),
        KnownNat n,
        1 <= n,
        SupportedPrim b,
        forall n. (KnownNat n, 1 <= n) => SupportedPrim (bv2 n)
      ) =>
      Term (bv n) ->
      TypeRep n ->
      TypeRep b ->
      TypeRep bv2 ->
      SomeTerm
    fromBV t1 tn tb tbv2 =
      case (eqTypeRep tn (typeRep @1), eqTypeRep tb (typeRep @Bool)) of
        (Just HRefl, Just HRefl) -> someTerm (bitCastTerm t1 :: Term b)
        _ -> case tb of
          (App (tw'@(Con _) :: TypeRep w') (tn' :: TypeRep n')) ->
            case ( eqTypeRep tw' tbv2,
                   eqTypeRep tn tn'
                 ) of
              (Just HRefl, Just HRefl) ->
                someTerm (bitCastTerm t1 :: Term b)
              _ -> err
          ( App
              (App (tw' :: TypeRep f) (teb :: TypeRep eb))
              (tsb :: TypeRep sb)
            ) ->
              withPrim @b $
                case (eqTypeRep tw' (typeRep @FP)) of
                  Just HRefl ->
                    if natVal teb + natVal tsb == natVal tn
                      then case unsafeAxiom @n @(eb + sb) of
                        Refl -> someTerm (bitCastTerm t1 :: Term b)
                      else err
                  _ -> err
          _ -> err

constructBVConcatTerm :: (HasCallStack) => SomeTerm -> SomeTerm -> SomeTerm
constructBVConcatTerm (SomeTerm (ta :: Term a)) (SomeTerm (tb :: Term b)) =
  withPrim @a $
    withPrim @b $
      case (tra, trb) of
        ( App (trbv0@(Con _) :: TypeRep bv0) (trn0 :: TypeRep n0),
          App (trbv1@(Con _) :: TypeRep bv1) (trn1 :: TypeRep n1)
          ) ->
            case ( eqTypeRep trbv0 trbv1,
                   eqTypeRep trbv0 (typeRep @WordN),
                   eqTypeRep trbv0 (typeRep @IntN)
                 ) of
              (Just HRefl, Just HRefl, _) ->
                case ( unsafeLeqProof @1 @(n0 + n1),
                       unsafeKnownProof @(n0 + n1) (natVal trn0 + natVal trn1)
                     ) of
                  (LeqProof, KnownProof) -> someTerm $ bvConcatTerm ta tb
              (Just HRefl, _, Just HRefl) ->
                case ( unsafeLeqProof @1 @(n0 + n1),
                       unsafeKnownProof @(n0 + n1) (natVal trn0 + natVal trn1)
                     ) of
                  (LeqProof, KnownProof) -> someTerm $ bvConcatTerm ta tb
              _ -> err
        _ -> err
  where
    tra = primTypeRep @a
    trb = primTypeRep @b
    err :: SomeTerm
    err =
      error $
        "constructBVConcatTerm: unsupported type: "
          <> show (primTypeRep @a)

constructBVSelectTerm ::
  (HasCallStack) => Natural -> Natural -> SomeTerm -> SomeTerm
constructBVSelectTerm ix w (SomeTerm (ta :: Term a)) =
  withPrim @a $
    case tra of
      (App (trbv@(Con _) :: TypeRep bv) (trn :: TypeRep n)) ->
        case ( eqTypeRep trbv (typeRep @WordN),
               eqTypeRep trbv (typeRep @IntN)
             ) of
          (Just HRefl, _) -> case (mkNatRepr ix, mkPositiveNatRepr w) of
            ( SomeNatRepr (nix :: NatRepr ix),
              SomePositiveNatRepr (nw :: NatRepr w)
              ) ->
                if ix + w <= natVal trn
                  then case unsafeLeqProof @(ix + w) @n of
                    LeqProof -> someTerm $ bvSelectTerm nix nw ta
                  else err
          (_, Just HRefl) -> case (mkNatRepr ix, mkPositiveNatRepr w) of
            ( SomeNatRepr (nix :: NatRepr ix),
              SomePositiveNatRepr (nw :: NatRepr w)
              ) ->
                if ix + w <= natVal trn
                  then case unsafeLeqProof @(ix + w) @n of
                    LeqProof -> someTerm $ bvSelectTerm nix nw ta
                  else err
          _ -> err
      _ -> err
  where
    tra = primTypeRep @a
    err :: SomeTerm
    err =
      error $
        "constructBVSelectTerm: unsupported type: "
          <> show (primTypeRep @a)

constructBVExtendTerm ::
  (HasCallStack) => Bool -> Natural -> SomeTerm -> SomeTerm
constructBVExtendTerm signed r (SomeTerm (ta :: Term a)) =
  withPrim @a $
    case tra of
      (App (trbv@(Con _) :: TypeRep bv) (trn :: TypeRep n)) ->
        case ( eqTypeRep trbv (typeRep @WordN),
               eqTypeRep trbv (typeRep @IntN)
             ) of
          (Just HRefl, _) -> case (mkPositiveNatRepr r) of
            (SomePositiveNatRepr (nr :: NatRepr r)) ->
              if natVal trn <= r
                then case unsafeLeqProof @n @r of
                  LeqProof -> someTerm $ bvExtendTerm signed nr ta
                else err
          (_, Just HRefl) -> case (mkPositiveNatRepr r) of
            (SomePositiveNatRepr (nr :: NatRepr r)) ->
              if natVal trn <= r
                then case unsafeLeqProof @n @r of
                  LeqProof -> someTerm $ bvExtendTerm signed nr ta
                else err
          _ -> err
      _ -> err
  where
    tra = primTypeRep @a
    err :: SomeTerm
    err =
      error $
        "constructBVExtendTerm: unsupported type: "
          <> show (primTypeRep @a)

constructApplyTerm ::
  (HasCallStack) => SomeTerm -> SomeTerm -> SomeTerm
constructApplyTerm (SomeTerm (tf :: Term f)) (SomeTerm (ta :: Term a)) =
  withPrim @f $
    withPrim @a $
      case trf of
        (App (App (trft :: TypeRep ft) (trarg :: TypeRep arg)) _) ->
          case ( eqTypeRep trft (typeRep @(=->)),
                 eqTypeRep trft (typeRep @(-->))
               ) of
            (Just HRefl, _) -> case eqTypeRep trarg tra of
              Just HRefl -> someTerm $ applyTerm tf ta
              Nothing -> err
            (_, Just HRefl) -> case eqTypeRep trarg tra of
              Just HRefl -> someTerm $ applyTerm tf ta
              Nothing -> err
            _ -> err
        _ -> err
  where
    trf = primTypeRep @f
    tra = primTypeRep @a
    err :: SomeTerm
    err =
      error $
        "constructApplyTerm: unsupported type: " <> show trf <> show tra

constructFromIntegralTerm' ::
  forall a.
  ( HasCallStack,
    PEvalFromIntegralTerm a Integer,
    PEvalFromIntegralTerm a AlgReal,
    forall n. (KnownNat n, 1 <= n) => PEvalFromIntegralTerm a (WordN n),
    forall n. (KnownNat n, 1 <= n) => PEvalFromIntegralTerm a (IntN n),
    forall eb sb. (ValidFP eb sb) => PEvalFromIntegralTerm a (FP eb sb)
  ) =>
  Term a ->
  KnownType ->
  SomeTerm
constructFromIntegralTerm' ta@SupportedTerm retType =
  case witnessKnownType retType of
    KnownTypeWitness (_ :: Proxy b) -> do
      let tb = primTypeRep @b
      withPrim @a $
        withPrim @b $
          case ( eqTypeRep tb (typeRep @Integer),
                 eqTypeRep tb (typeRep @AlgReal),
                 tb
               ) of
            (Just HRefl, _, _) -> someTerm (fromIntegralTerm ta :: Term b)
            (_, Just HRefl, _) -> someTerm (fromIntegralTerm ta :: Term b)
            (_, _, App tw@Con {} _) ->
              case ( eqTypeRep tw (typeRep @WordN),
                     eqTypeRep tw (typeRep @IntN)
                   ) of
                (Just HRefl, _) -> someTerm (fromIntegralTerm ta :: Term b)
                (_, Just HRefl) -> someTerm (fromIntegralTerm ta :: Term b)
                _ -> err
            (_, _, App (App tw@Con {} _) _) ->
              case eqTypeRep tw (typeRep @FP) of
                Just HRefl -> someTerm (fromIntegralTerm ta :: Term b)
                _ -> err
            _ -> err
  where
    err :: SomeTerm
    err =
      error $
        "constructFromIntegralTerm: unsupported type: "
          <> show (primTypeRep @a)
          <> show retType

constructFromIntegralTerm ::
  (HasCallStack) => SomeTerm -> KnownType -> SomeTerm
constructFromIntegralTerm (SomeTerm (t1 :: Term a)) retType =
  withPrim @a $
    case (eqTypeRep tra (typeRep @Integer), tra) of
      (Just HRefl, _) -> constructFromIntegralTerm' t1 retType
      (_, App (trbv :: TypeRep bv) _) ->
        case ( eqTypeRep trbv (typeRep @WordN),
               eqTypeRep trbv (typeRep @IntN)
             ) of
          (Just HRefl, _) -> constructFromIntegralTerm' t1 retType
          (_, Just HRefl) -> constructFromIntegralTerm' t1 retType
          _ -> err
      _ -> err
  where
    tra = primTypeRep @a
    err :: SomeTerm
    err = error $ "constructFromIntegralTerm: unsupported type: " <> show tra

knownTypeTermId :: Term a -> (KnownType, Id)
knownTypeTermId t@SupportedTerm = (knownType t, termId t)

statefulDeserializeSomeTerm ::
  (MonadGet m) =>
  StateT (HM.HashMap (KnownType, Id) SomeTerm, SomeTerm) m SomeTerm
statefulDeserializeSomeTerm = do
  r <- do
    ktTmId <- deserialize
    tag <- getWord8
    if
      | tag == conTermTag -> do
          knownType <- deserializeKnownType
          case witnessKnownType knownType of
            KnownTypeWitness (Proxy :: Proxy a) -> do
              tm <- someTerm . conTerm <$> deserialize @a
              return $ Just (tm, ktTmId)
      | tag == symTermTag -> do
          SomeTypedSymbol sb <- deserialize @SomeTypedAnySymbol
          return $ Just (someTerm $ symTerm sb, ktTmId)
      | tag == forallTermTag -> deserializeQuantified ktTmId forallTerm
      | tag == existsTermTag -> deserializeQuantified ktTmId existsTerm
      | tag == notTermTag -> do
          t <- deserializeTerm
          return $ Just (someTerm $ notTerm $ asBoolTerm t, ktTmId)
      | tag == orTermTag -> deserializeBoolBinary ktTmId orTerm
      | tag == andTermTag -> deserializeBoolBinary ktTmId andTerm
      | tag == eqTermTag -> do
          SomeTerm t1' <- deserializeTerm
          t2 <- deserializeTerm
          asSameType t1' t2 $ \t2' ->
            return $ Just (someTerm $ eqTerm t1' t2', ktTmId)
      | tag == distinctTermTag -> do
          ts <- deserializeNonEmptyTermList
          asSameTypeNonEmptyTermList ts $ \ts' ->
            return $ Just (someTerm $ distinctTerm ts', ktTmId)
      | tag == iteTermTag -> do
          t1 <- deserializeTerm
          SomeTerm t2' <- deserializeTerm
          t3 <- deserializeTerm
          asSameType t2' t3 $ \t3' ->
            return $ Just (someTerm $ iteTerm (asBoolTerm t1) t2' t3', ktTmId)
      | tag == addNumTermTag -> deserializeNumBinary ktTmId addNumTerm
      | tag == negNumTermTag -> deserializeNumUnary ktTmId negNumTerm
      | tag == mulNumTermTag -> deserializeNumBinary ktTmId mulNumTerm
      | tag == absNumTermTag -> deserializeNumUnary ktTmId absNumTerm
      | tag == signumNumTermTag -> deserializeNumUnary ktTmId signumNumTerm
      | tag == ltOrdTermTag -> deserializeOrdBinary ktTmId ltOrdTerm
      | tag == leOrdTermTag -> deserializeOrdBinary ktTmId leOrdTerm
      | tag == andBitsTermTag -> deserializeBitsBinary ktTmId andBitsTerm
      | tag == orBitsTermTag -> deserializeBitsBinary ktTmId orBitsTerm
      | tag == xorBitsTermTag -> deserializeBitsBinary ktTmId xorBitsTerm
      | tag == complementBitsTermTag ->
          deserializeBitsUnary ktTmId complementBitsTerm
      | tag == shiftLeftTermTag -> deserializeBitsBinary ktTmId shiftLeftTerm
      | tag == shiftRightTermTag -> deserializeBitsBinary ktTmId shiftRightTerm
      | tag == rotateLeftTermTag -> deserializeBitsBinary ktTmId rotateLeftTerm
      | tag == rotateRightTermTag -> deserializeBitsBinary ktTmId rotateRightTerm
      | tag == bitCastTermTag -> do
          kt <- deserializeKnownType
          t1 <- deserializeTerm
          return $ Just (constructBitCastTerm t1 kt, ktTmId)
      | tag == bitCastOrTermTag -> do
          td <- deserializeTerm
          tv <- deserializeTerm
          return $ Just (constructBitCastOrTerm td tv, ktTmId)
      | tag == bvConcatTermTag -> do
          t1 <- deserializeTerm
          t2 <- deserializeTerm
          return $ Just (constructBVConcatTerm t1 t2, ktTmId)
      | tag == bvSelectTermTag -> do
          ix <- deserialize @Natural
          w <- deserialize @Natural
          t1 <- deserializeTerm
          return $ Just (constructBVSelectTerm ix w t1, ktTmId)
      | tag == bvExtendTermTag -> do
          signed <- deserialize @Bool
          r <- deserialize @Natural
          t1 <- deserializeTerm
          return $ Just (constructBVExtendTerm signed r t1, ktTmId)
      | tag == applyTermTag -> do
          tf <- deserializeTerm
          ta <- deserializeTerm
          return $ Just (constructApplyTerm tf ta, ktTmId)
      | tag == divIntegralTermTag ->
          deserializeIntegralBinary ktTmId divIntegralTerm
      | tag == modIntegralTermTag ->
          deserializeIntegralBinary ktTmId modIntegralTerm
      | tag == quotIntegralTermTag ->
          deserializeIntegralBinary ktTmId quotIntegralTerm
      | tag == remIntegralTermTag ->
          deserializeIntegralBinary ktTmId remIntegralTerm
      | tag == terminalTag -> return Nothing
      | tag == fpTraitTermTag -> do
          trait <- deserialize @FPTrait
          t <- deserializeTerm
          asFPTypeTerm t $ \t' ->
            return $ Just (someTerm $ fpTraitTerm trait t', ktTmId)
      | tag == fdivTermTag -> do
          t1 <- deserializeTerm
          t2 <- deserializeTerm
          asFloatingFractionalTypeTerm t1 $ \t1' -> asSameType t1' t2 $ \t2' ->
            return $ Just (someTerm $ fdivTerm t1' t2', ktTmId)
      | tag == recipTermTag -> do
          t <- deserializeTerm
          asFloatingFractionalTypeTerm t $ \t' ->
            return $ Just (someTerm $ recipTerm t', ktTmId)
      | tag == floatingUnaryTermTag -> do
          op <- deserialize
          t <- deserializeTerm
          asFloatingFractionalTypeTerm t $ \t' ->
            return $ Just (someTerm $ floatingUnaryTerm op t', ktTmId)
      | tag == powerTermTag -> do
          t1 <- deserializeTerm
          t2 <- deserializeTerm
          asFloatingFractionalTypeTerm t1 $ \t1' -> asSameType t1' t2 $ \t2' ->
            return $ Just (someTerm $ powerTerm t1' t2', ktTmId)
      | tag == fpUnaryTermTag -> do
          op <- deserialize @FPUnaryOp
          t <- deserializeTerm
          asFPTypeTerm t $ \t' ->
            return $ Just (someTerm $ fpUnaryTerm op t', ktTmId)
      | tag == fpBinaryTermTag -> do
          op <- deserialize @FPBinaryOp
          t1 <- deserializeTerm
          t2 <- deserializeTerm
          asFPTypeTerm t1 $ \t1' -> asSameType t1' t2 $ \t2' ->
            return $ Just (someTerm $ fpBinaryTerm op t1' t2', ktTmId)
      | tag == fpRoundingUnaryTermTag -> do
          op <- deserialize @FPRoundingUnaryOp
          trd <- deserializeTerm
          t <- deserializeTerm
          asFPRoundingTerm trd $ \trd' ->
            asFPTypeTerm t $ \t' ->
              return $ Just (someTerm $ fpRoundingUnaryTerm op trd' t', ktTmId)
      | tag == fpRoundingBinaryTermTag -> do
          op <- deserialize @FPRoundingBinaryOp
          trd <- deserializeTerm
          t1 <- deserializeTerm
          t2 <- deserializeTerm
          asFPRoundingTerm trd $ \trd' ->
            asFPTypeTerm t1 $ \t1' -> asSameType t1' t2 $ \t2' ->
              return $
                Just (someTerm $ fpRoundingBinaryTerm op trd' t1' t2', ktTmId)
      | tag == fpFMATermTag -> do
          trd <- deserializeTerm
          t1 <- deserializeTerm
          t2 <- deserializeTerm
          t3 <- deserializeTerm
          asFPRoundingTerm trd $ \trd' -> asFPTypeTerm t1 $ \t1' ->
            asSameType t1' t2 $ \t2' -> asSameType t1' t3 $ \t3' ->
              return $ Just (someTerm $ fpFMATerm trd' t1' t2' t3', ktTmId)
      | tag == fromIntegralTermTag -> do
          kt <- deserializeKnownType
          t <- deserializeTerm
          return $ Just (constructFromIntegralTerm t kt, ktTmId)
      | tag == fromFPOrTermTag -> do
          td <- deserializeTerm
          trd <- deserializeTerm
          tt <- deserializeTerm
          asNumTypeTerm td $ \td' -> asFPRoundingTerm trd $ \trd' ->
            asFPTypeTerm tt $ \tt' ->
              return $ Just (someTerm $ fromFPOrTerm td' trd' tt', ktTmId)
      | tag == toFPTermTag -> do
          eb <- deserialize @Natural
          sb <- deserialize @Natural
          trd <- deserializeTerm
          tt <- deserializeTerm
          if checkDynamicValidFP eb sb
            then case (mkNatRepr eb, mkNatRepr sb) of
              ( SomeNatRepr (_ :: NatRepr eb),
                SomeNatRepr (_ :: NatRepr sb)
                ) ->
                  withUnsafeValidFP @eb @sb $
                    asFPRoundingTerm trd $ \trd' -> asNumTypeTerm tt $ \tt' ->
                      return $
                        Just
                          ( someTerm (toFPTerm trd' tt' :: Term (FP eb sb)),
                            ktTmId
                          )
            else error "statefulDeserializeSomeTerm: invalid FP type"
      | otherwise ->
          error $ "statefulDeserializeSomeTerm: unknown tag: " <> show tag
  case r of
    Just (tm, ktTmId) -> do
      State.modify' $ \(m, _) -> (HM.insert ktTmId tm m, tm)
      statefulDeserializeSomeTerm
    Nothing -> State.gets snd
  where
    deserializeNonEmptyTermList ::
      (MonadGet m) =>
      StateT
        (HM.HashMap (KnownType, Id) SomeTerm, SomeTerm)
        m
        (NonEmpty SomeTerm)
    deserializeNonEmptyTermList = do
      ids <- deserialize @[(KnownType, Id)]
      case ids of
        [] -> fail "statefulDeserializeSomeTerm: empty list"
        (x : xs) -> do
          x' <- queryTerm x
          xs' <- traverse queryTerm xs
          return $ x' :| xs'
    deserializeTerm ::
      (MonadGet m) =>
      StateT (HM.HashMap (KnownType, Id) SomeTerm, SomeTerm) m SomeTerm
    deserializeTerm = do
      ktTmId <- deserialize
      queryTerm ktTmId
    queryTerm ::
      (MonadGet m) =>
      (KnownType, Id) ->
      StateT (HM.HashMap (KnownType, Id) SomeTerm, SomeTerm) m SomeTerm
    queryTerm ktTmId = do
      tm <- State.gets $ HM.lookup ktTmId . fst
      case tm of
        Nothing -> fail "statefulDeserializeSomeTerm: unknown term id"
        Just tm' -> return tm'
    deserializeBoolBinary tmId f = do
      t1 <- deserializeTerm
      t2 <- deserializeTerm
      return $
        Just (someTerm $ f (asBoolTerm t1) (asBoolTerm t2), tmId)
    deserializeQuantified
      tmId
      (f :: forall t. TypedConstantSymbol t -> Term Bool -> Term Bool) = do
        SomeTypedSymbol sb <- deserialize
        t <- deserializeTerm
        return $ Just (someTerm $ f sb $ asBoolTerm t, tmId)
    deserializeNumUnary
      tmId
      (f :: forall t. (PEvalNumTerm t) => Term t -> Term t) = do
        t1 <- deserializeTerm
        asNumTypeTerm t1 $ \t1' -> return $ Just (someTerm $ f t1', tmId)
    deserializeBitsUnary
      tmId
      (f :: forall t. (PEvalBitwiseTerm t) => Term t -> Term t) = do
        t1 <- deserializeTerm
        asBitsTypeTerm t1 $ \t1' -> return $ Just (someTerm $ f t1', tmId)
    deserializeNumBinary
      tmId
      (f :: forall t. (PEvalNumTerm t) => Term t -> Term t -> Term t) = do
        t1 <- deserializeTerm
        t2 <- deserializeTerm
        asNumTypeTerm t1 $ \t1' -> asSameType t1' t2 $ \t2' ->
          return $ Just (someTerm $ f t1' t2', tmId)
    deserializeOrdBinary
      tmId
      (f :: forall t. (PEvalOrdTerm t) => Term t -> Term t -> Term Bool) = do
        t1 <- deserializeTerm
        t2 <- deserializeTerm
        asOrdTypeTerm t1 $ \t1' -> asSameType t1' t2 $ \t2' ->
          return $ Just (someTerm $ f t1' t2', tmId)
    deserializeBitsBinary
      tmId
      ( f ::
          forall t.
          ( PEvalBitwiseTerm t,
            PEvalShiftTerm t,
            PEvalRotateTerm t
          ) =>
          Term t ->
          Term t ->
          Term t
        ) = do
        t1 <- deserializeTerm
        t2 <- deserializeTerm
        asBitsTypeTerm t1 $ \t1' -> asSameType t1' t2 $ \t2' ->
          return $ Just (someTerm $ f t1' t2', tmId)
    deserializeIntegralBinary
      tmId
      ( f ::
          forall t.
          (PEvalDivModIntegralTerm t) =>
          Term t ->
          Term t ->
          Term t
        ) = do
        t1 <- deserializeTerm
        t2 <- deserializeTerm
        asIntegralTypeTerm t1 $ \t1' -> asSameType t1' t2 $ \t2' ->
          return $ Just (someTerm $ f t1' t2', tmId)

deserializeSomeTerm :: (MonadGet m) => m SomeTerm
deserializeSomeTerm =
  evalStateT
    statefulDeserializeSomeTerm
    ( HM.empty,
      error $
        "deserializeSomeTerm: should not happen: started with the terminal "
          <> "value"
    )

serializeSingleSomeTerm ::
  (MonadPut m) => SomeTerm -> StateT (HS.HashSet (KnownType, Id)) m ()
serializeSingleSomeTerm (SomeTerm (tm :: Term t)) = do
  st <- State.get
  let kt = knownType tm
  let tmId = termId tm
  let ktTmId = (kt, tmId)
  if HS.member ktTmId st
    then return ()
    else do
      case tm of
        ConTerm (v :: v) -> do
          serialize ktTmId
          putWord8 conTermTag
          let kt = knownType (Proxy @v)
          case witnessKnownType kt of
            KnownTypeWitness (Proxy :: Proxy v1) ->
              case eqTypeRep (primTypeRep @v) (typeRep @v1) of
                Just HRefl -> do
                  serializeKnownType kt
                  serialize v
                Nothing ->
                  error
                    "serializeSingleSomeTerm: should not happen: type mismatch"
        SymTerm (v :: TypedAnySymbol v) -> do
          serialize ktTmId
          putWord8 symTermTag
          serialize $ someTypedSymbol v
        ForallTerm ts t -> serializeQuantified ktTmId forallTermTag ts t
        ExistsTerm ts t -> serializeQuantified ktTmId existsTermTag ts t
        NotTerm t -> do
          serializeSingleSomeTerm $ someTerm t
          serialize ktTmId
          putWord8 notTermTag
          serialize $ knownTypeTermId t
        OrTerm t1 t2 -> serializeBinary ktTmId orTermTag t1 t2
        AndTerm t1 t2 -> serializeBinary ktTmId andTermTag t1 t2
        EqTerm t1 t2 -> serializeBinary ktTmId eqTermTag t1 t2
        DistinctTerm ts -> do
          traverse_ (serializeSingleSomeTerm . someTerm) ts
          serialize ktTmId
          putWord8 distinctTermTag
          serialize $ fmap knownTypeTermId ts
        ITETerm t1 t2 t3 -> serializeTernary ktTmId iteTermTag t1 t2 t3
        AddNumTerm t1 t2 -> serializeBinary ktTmId addNumTermTag t1 t2
        NegNumTerm t -> serializeUnary ktTmId negNumTermTag t
        MulNumTerm t1 t2 -> serializeBinary ktTmId mulNumTermTag t1 t2
        AbsNumTerm t -> serializeUnary ktTmId absNumTermTag t
        SignumNumTerm t -> serializeUnary ktTmId signumNumTermTag t
        LtOrdTerm t1 t2 -> serializeBinary ktTmId ltOrdTermTag t1 t2
        LeOrdTerm t1 t2 -> serializeBinary ktTmId leOrdTermTag t1 t2
        AndBitsTerm t1 t2 -> serializeBinary ktTmId andBitsTermTag t1 t2
        OrBitsTerm t1 t2 -> serializeBinary ktTmId orBitsTermTag t1 t2
        XorBitsTerm t1 t2 -> serializeBinary ktTmId xorBitsTermTag t1 t2
        ComplementBitsTerm t ->
          serializeUnary ktTmId complementBitsTermTag t
        ShiftLeftTerm t1 t2 ->
          serializeBinary ktTmId shiftLeftTermTag t1 t2
        ShiftRightTerm t1 t2 ->
          serializeBinary ktTmId shiftRightTermTag t1 t2
        RotateLeftTerm t1 t2 ->
          serializeBinary ktTmId rotateLeftTermTag t1 t2
        RotateRightTerm t1 t2 ->
          serializeBinary ktTmId rotateRightTermTag t1 t2
        BitCastTerm t -> do
          serializeSingleSomeTerm $ someTerm t
          serialize ktTmId
          serialize bitCastTermTag
          let kt = knownType (Proxy @t)
          serializeKnownType kt
          serialize $ knownTypeTermId t
        BitCastOrTerm d t -> serializeBinary ktTmId bitCastOrTermTag d t
        BVConcatTerm t1 t2 -> serializeBinary ktTmId bvConcatTermTag t1 t2
        BVSelectTerm ix w t -> do
          serializeSingleSomeTerm $ someTerm t
          serialize ktTmId
          serialize bvSelectTermTag
          serialize $ natVal ix
          serialize $ natVal w
          serialize $ knownTypeTermId t
        BVExtendTerm signed r t -> do
          serializeSingleSomeTerm $ someTerm t
          serialize ktTmId
          serialize bvExtendTermTag
          serialize signed
          serialize $ natVal r
          serialize $ knownTypeTermId t
        ApplyTerm f ts -> serializeBinary ktTmId applyTermTag f ts
        DivIntegralTerm t1 t2 ->
          serializeBinary ktTmId divIntegralTermTag t1 t2
        ModIntegralTerm t1 t2 ->
          serializeBinary ktTmId modIntegralTermTag t1 t2
        QuotIntegralTerm t1 t2 ->
          serializeBinary ktTmId quotIntegralTermTag t1 t2
        RemIntegralTerm t1 t2 ->
          serializeBinary ktTmId remIntegralTermTag t1 t2
        FPTraitTerm trait t -> do
          serializeSingleSomeTerm $ someTerm t
          serialize ktTmId
          serialize fpTraitTermTag
          serialize trait
          serialize $ knownTypeTermId t
        FdivTerm t1 t2 -> serializeBinary ktTmId fdivTermTag t1 t2
        RecipTerm t -> serializeUnary ktTmId recipTermTag t
        FloatingUnaryTerm op t -> do
          serializeSingleSomeTerm $ someTerm t
          serialize ktTmId
          serialize floatingUnaryTermTag
          serialize op
          serialize $ knownTypeTermId t
        PowerTerm t1 t2 -> serializeBinary ktTmId powerTermTag t1 t2
        FPUnaryTerm op t -> do
          serializeSingleSomeTerm $ someTerm t
          serialize ktTmId
          serialize fpUnaryTermTag
          serialize op
          serialize $ knownTypeTermId t
        FPBinaryTerm op t1 t2 -> do
          serializeSingleSomeTerm $ someTerm t1
          serializeSingleSomeTerm $ someTerm t2
          serialize ktTmId
          serialize fpBinaryTermTag
          serialize op
          serialize $ knownTypeTermId t1
          serialize $ knownTypeTermId t2
        FPRoundingUnaryTerm op rd t -> do
          serializeSingleSomeTerm $ someTerm rd
          serializeSingleSomeTerm $ someTerm t
          serialize ktTmId
          serialize fpRoundingUnaryTermTag
          serialize op
          serialize $ knownTypeTermId rd
          serialize $ knownTypeTermId t
        FPRoundingBinaryTerm op rd t1 t2 -> do
          serializeSingleSomeTerm $ someTerm rd
          serializeSingleSomeTerm $ someTerm t1
          serializeSingleSomeTerm $ someTerm t2
          serialize ktTmId
          serialize fpRoundingBinaryTermTag
          serialize op
          serialize $ knownTypeTermId rd
          serialize $ knownTypeTermId t1
          serialize $ knownTypeTermId t2
        FPFMATerm rd t1 t2 t3 -> do
          serializeSingleSomeTerm $ someTerm rd
          serializeSingleSomeTerm $ someTerm t1
          serializeSingleSomeTerm $ someTerm t2
          serializeSingleSomeTerm $ someTerm t3
          serialize ktTmId
          serialize fpFMATermTag
          serialize $ knownTypeTermId rd
          serialize $ knownTypeTermId t1
          serialize $ knownTypeTermId t2
          serialize $ knownTypeTermId t3
        FromIntegralTerm t -> do
          serializeSingleSomeTerm $ someTerm t
          serialize ktTmId
          serialize fromIntegralTermTag
          let kt = knownType (Proxy @t)
          serializeKnownType kt
          serialize $ knownTypeTermId t
        FromFPOrTerm d rd t ->
          serializeTernary ktTmId fromFPOrTermTag d rd t
        ToFPTerm rd t eb sb -> do
          serializeSingleSomeTerm $ someTerm rd
          serializeSingleSomeTerm $ someTerm t
          serialize ktTmId
          serialize toFPTermTag
          serialize $ natVal eb
          serialize $ natVal sb
          serialize $ knownTypeTermId rd
          serialize $ knownTypeTermId t
  State.put $ HS.insert ktTmId st
  where
    serializeQuantified ::
      (MonadPut m) =>
      (KnownType, Id) ->
      Word8 ->
      TypedConstantSymbol v ->
      Term b ->
      StateT (HS.HashSet (KnownType, Id)) m ()
    serializeQuantified ktTmId tag v t = do
      serializeSingleSomeTerm $ someTerm t
      serialize ktTmId
      serialize tag
      serialize $ someTypedSymbol v
      serialize $ knownTypeTermId t
    serializeUnary ktTmId tag t1 = do
      serializeSingleSomeTerm $ someTerm t1
      serialize ktTmId
      serialize tag
      serialize $ knownTypeTermId t1
    serializeBinary ktTmId tag t1 t2 = do
      serializeSingleSomeTerm $ someTerm t1
      serializeSingleSomeTerm $ someTerm t2
      serialize ktTmId
      serialize tag
      serialize $ knownTypeTermId t1
      serialize $ knownTypeTermId t2
    serializeTernary ktTmId tag t1 t2 t3 = do
      serializeSingleSomeTerm $ someTerm t1
      serializeSingleSomeTerm $ someTerm t2
      serializeSingleSomeTerm $ someTerm t3
      serialize ktTmId
      serialize tag
      serialize $ knownTypeTermId t1
      serialize $ knownTypeTermId t2
      serialize $ knownTypeTermId t3

serializeSomeTerm :: (MonadPut m) => SomeTerm -> m ()
serializeSomeTerm t = do
  flip evalStateT HS.empty $ serializeSingleSomeTerm t
  serialize (NonFuncType BoolType, 0 :: Id)
  putWord8 terminalTag

instance Serial SomeTerm where
  serialize = serializeSomeTerm
  deserialize = deserializeSomeTerm

instance Cereal.Serialize SomeTerm where
  put = serializeSomeTerm
  get = deserializeSomeTerm

instance Binary.Binary SomeTerm where
  put = serializeSomeTerm
  get = deserializeSomeTerm

instance (SupportedPrim a) => Serial (Term a) where
  serialize = serializeSomeTerm . someTerm
  deserialize = do
    SomeTerm tm <- deserialize
    case castTerm tm of
      Just r -> return r
      Nothing -> fail "deserialize Term: type mismatch"

instance (SupportedPrim a) => Cereal.Serialize (Term a) where
  put = serialize
  get = deserialize

instance (SupportedPrim a) => Binary.Binary (Term a) where
  put = serialize
  get = deserialize

instance Serial ModelValue where
  serialize (ModelValue (v :: v)) = do
    let kt = knownType (Proxy @v)
    serializeKnownType kt
    case witnessKnownType kt of
      KnownTypeWitness (Proxy :: Proxy v1) ->
        case eqTypeRep (primTypeRep @v) (typeRep @v1) of
          Just HRefl -> serialize v
          Nothing ->
            error
              "serialize ModelValue: should not happen: type mismatch"
  deserialize = do
    kt <- deserializeKnownType
    case witnessKnownType kt of
      KnownTypeWitness (Proxy :: Proxy v) -> do
        v <- deserialize @v
        return $ ModelValue v

instance Cereal.Serialize ModelValue where
  put = serialize
  get = deserialize

instance Binary.Binary ModelValue where
  put = serialize
  get = deserialize

instance (GeneralFunArg a, GeneralFunArg b) => Serial (a --> b) where
  serialize (GeneralFun ts tm) = serialize ts >> serialize tm
  deserialize = GeneralFun <$> deserialize <*> deserialize

instance (GeneralFunArg a, GeneralFunArg b) => Cereal.Serialize (a --> b) where
  put = serialize
  get = deserialize

instance (GeneralFunArg a, GeneralFunArg b) => Binary.Binary (a --> b) where
  put = serialize
  get = deserialize

type GeneralFunArg t = (SupportedNonFuncPrim t, Typeable t, Show t, Hashable t)

instance
  {-# OVERLAPPING #-}
  (GeneralFunArg a, GeneralFunArg b, GeneralFunArg c) =>
  Serial (a --> b --> c)
  where
  serialize (GeneralFun ts tm) = serialize ts >> serialize tm
  deserialize = GeneralFun <$> deserialize <*> deserialize

instance
  {-# OVERLAPPING #-}
  (GeneralFunArg a, GeneralFunArg b, GeneralFunArg c) =>
  Cereal.Serialize (a --> b --> c)
  where
  put = serialize
  get = deserialize

instance
  {-# OVERLAPPING #-}
  (GeneralFunArg a, GeneralFunArg b, GeneralFunArg c) =>
  Binary.Binary (a --> b --> c)
  where
  put = serialize
  get = deserialize

instance
  {-# OVERLAPPING #-}
  (GeneralFunArg a, GeneralFunArg b, GeneralFunArg c, GeneralFunArg d) =>
  Serial (a --> b --> c --> d)
  where
  serialize (GeneralFun ts tm) = serialize ts >> serialize tm
  deserialize = GeneralFun <$> deserialize <*> deserialize

instance
  {-# OVERLAPPING #-}
  (GeneralFunArg a, GeneralFunArg b, GeneralFunArg c, GeneralFunArg d) =>
  Cereal.Serialize (a --> b --> c --> d)
  where
  put = serialize
  get = deserialize

instance
  {-# OVERLAPPING #-}
  (GeneralFunArg a, GeneralFunArg b, GeneralFunArg c, GeneralFunArg d) =>
  Binary.Binary (a --> b --> c --> d)
  where
  put = serialize
  get = deserialize

instance
  {-# OVERLAPPING #-}
  ( GeneralFunArg a,
    GeneralFunArg b,
    GeneralFunArg c,
    GeneralFunArg d,
    GeneralFunArg e
  ) =>
  Serial (a --> b --> c --> d --> e)
  where
  serialize (GeneralFun ts tm) = serialize ts >> serialize tm
  deserialize = GeneralFun <$> deserialize <*> deserialize

instance
  {-# OVERLAPPING #-}
  ( GeneralFunArg a,
    GeneralFunArg b,
    GeneralFunArg c,
    GeneralFunArg d,
    GeneralFunArg e
  ) =>
  Cereal.Serialize (a --> b --> c --> d --> e)
  where
  put = serialize
  get = deserialize

instance
  {-# OVERLAPPING #-}
  ( GeneralFunArg a,
    GeneralFunArg b,
    GeneralFunArg c,
    GeneralFunArg d,
    GeneralFunArg e
  ) =>
  Binary.Binary (a --> b --> c --> d --> e)
  where
  put = serialize
  get = deserialize

instance
  {-# OVERLAPPING #-}
  ( GeneralFunArg a,
    GeneralFunArg b,
    GeneralFunArg c,
    GeneralFunArg d,
    GeneralFunArg e,
    GeneralFunArg f
  ) =>
  Serial (a --> b --> c --> d --> e --> f)
  where
  serialize (GeneralFun ts tm) = serialize ts >> serialize tm
  deserialize = GeneralFun <$> deserialize <*> deserialize

instance
  {-# OVERLAPPING #-}
  ( GeneralFunArg a,
    GeneralFunArg b,
    GeneralFunArg c,
    GeneralFunArg d,
    GeneralFunArg e,
    GeneralFunArg f
  ) =>
  Cereal.Serialize (a --> b --> c --> d --> e --> f)
  where
  put = serialize
  get = deserialize

instance
  {-# OVERLAPPING #-}
  ( GeneralFunArg a,
    GeneralFunArg b,
    GeneralFunArg c,
    GeneralFunArg d,
    GeneralFunArg e,
    GeneralFunArg f
  ) =>
  Binary.Binary (a --> b --> c --> d --> e --> f)
  where
  put = serialize
  get = deserialize

instance
  {-# OVERLAPPING #-}
  ( GeneralFunArg a,
    GeneralFunArg b,
    GeneralFunArg c,
    GeneralFunArg d,
    GeneralFunArg e,
    GeneralFunArg f,
    GeneralFunArg g
  ) =>
  Serial (a --> b --> c --> d --> e --> f --> g)
  where
  serialize (GeneralFun ts tm) = serialize ts >> serialize tm
  deserialize = GeneralFun <$> deserialize <*> deserialize

instance
  {-# OVERLAPPING #-}
  ( GeneralFunArg a,
    GeneralFunArg b,
    GeneralFunArg c,
    GeneralFunArg d,
    GeneralFunArg e,
    GeneralFunArg f,
    GeneralFunArg g
  ) =>
  Cereal.Serialize (a --> b --> c --> d --> e --> f --> g)
  where
  put = serialize
  get = deserialize

instance
  {-# OVERLAPPING #-}
  ( GeneralFunArg a,
    GeneralFunArg b,
    GeneralFunArg c,
    GeneralFunArg d,
    GeneralFunArg e,
    GeneralFunArg f,
    GeneralFunArg g
  ) =>
  Binary.Binary (a --> b --> c --> d --> e --> f --> g)
  where
  put = serialize
  get = deserialize

instance
  {-# OVERLAPPING #-}
  ( GeneralFunArg a,
    GeneralFunArg b,
    GeneralFunArg c,
    GeneralFunArg d,
    GeneralFunArg e,
    GeneralFunArg f,
    GeneralFunArg g,
    GeneralFunArg h
  ) =>
  Serial (a --> b --> c --> d --> e --> f --> g --> h)
  where
  serialize (GeneralFun ts tm) = serialize ts >> serialize tm
  deserialize = GeneralFun <$> deserialize <*> deserialize

instance
  {-# OVERLAPPING #-}
  ( GeneralFunArg a,
    GeneralFunArg b,
    GeneralFunArg c,
    GeneralFunArg d,
    GeneralFunArg e,
    GeneralFunArg f,
    GeneralFunArg g,
    GeneralFunArg h
  ) =>
  Cereal.Serialize (a --> b --> c --> d --> e --> f --> g --> h)
  where
  put = serialize
  get = deserialize

instance
  {-# OVERLAPPING #-}
  ( GeneralFunArg a,
    GeneralFunArg b,
    GeneralFunArg c,
    GeneralFunArg d,
    GeneralFunArg e,
    GeneralFunArg f,
    GeneralFunArg g,
    GeneralFunArg h
  ) =>
  Binary.Binary (a --> b --> c --> d --> e --> f --> g --> h)
  where
  put = serialize
  get = deserialize

instance
  {-# OVERLAPPING #-}
  ( GeneralFunArg a,
    GeneralFunArg b,
    GeneralFunArg c,
    GeneralFunArg d,
    GeneralFunArg e,
    GeneralFunArg f,
    GeneralFunArg g,
    GeneralFunArg h,
    GeneralFunArg i
  ) =>
  Serial (a --> b --> c --> d --> e --> f --> g --> h --> i)
  where
  serialize (GeneralFun ts tm) = serialize ts >> serialize tm
  deserialize = GeneralFun <$> deserialize <*> deserialize

instance
  {-# OVERLAPPING #-}
  ( GeneralFunArg a,
    GeneralFunArg b,
    GeneralFunArg c,
    GeneralFunArg d,
    GeneralFunArg e,
    GeneralFunArg f,
    GeneralFunArg g,
    GeneralFunArg h,
    GeneralFunArg i
  ) =>
  Cereal.Serialize (a --> b --> c --> d --> e --> f --> g --> h --> i)
  where
  put = serialize
  get = deserialize

instance
  {-# OVERLAPPING #-}
  ( GeneralFunArg a,
    GeneralFunArg b,
    GeneralFunArg c,
    GeneralFunArg d,
    GeneralFunArg e,
    GeneralFunArg f,
    GeneralFunArg g,
    GeneralFunArg h,
    GeneralFunArg i
  ) =>
  Binary.Binary (a --> b --> c --> d --> e --> f --> g --> h --> i)
  where
  put = serialize
  get = deserialize
