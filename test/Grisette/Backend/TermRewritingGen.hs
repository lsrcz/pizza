{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Backend.TermRewritingGen
  ( TermRewritingSpec (..),
    GeneralSpec (..),
    DifferentSizeBVSpec (..),
    FixedSizedBVWithBoolSpec (..),
    BoolWithFixedSizedBVSpec (..),
    BoolWithLIASpec (..),
    LIAWithBoolSpec (..),
    BoolWithNRASpec (..),
    NRAWithBoolSpec (..),
    BoolOnlySpec (..),
    constructUnarySpec,
    constructBinarySpec,
    constructTernarySpec,
    divIntegralSpec,
    modIntegralSpec,
    quotIntegralSpec,
    remIntegralSpec,
    negNumSpec,
    mulNumSpec,
    addNumSpec,
    absNumSpec,
    signumNumSpec,
    ltOrdSpec,
    leOrdSpec,
    iteSpec,
    eqvSpec,
    distinctSpec,
    notSpec,
    andSpec,
    orSpec,
    shiftLeftSpec,
    shiftRightSpec,
    rotateLeftSpec,
    rotateRightSpec,
    fpTraitSpec,
    fdivSpec,
    recipSpec,
    floatingUnarySpec,
    powerSpec,
    fpUnaryOpSpec,
    fpBinaryOpSpec,
    fpRoundingUnaryOpSpec,
    fpRoundingBinarySpec,
    fpFMASpec,
    bitCastSpec,
    bitCastOrSpec,
    fromFPOrSpec,
    toFPSpec,
    bvconcatSpec,
    bvselectSpec,
    bvextendSpec,
    andBitsSpec,
    orBitsSpec,
    xorBitsSpec,
    complementBitsSpec,
    IEEEFPSpec (..),
    IEEEFPBoolOpSpec (..),
    FPRoundingModeSpec (..),
    FPRoundingModeBoolOpSpec (..),
  )
where

import Data.Bits (FiniteBits)
import Data.Data (Proxy (Proxy), Typeable)
import Data.Functor ((<&>))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Text as T
import GHC.TypeLits (KnownNat, Nat, type (+), type (<=))
import Grisette
  ( Identifier,
    SExpr (Atom),
    SizedBV,
    SymRotate,
    SymShift,
    withMetadata,
  )
import Grisette.Internal.SymPrim.AlgReal (AlgReal)
import Grisette.Internal.SymPrim.FP
  ( FP,
    FPRoundingMode (RNA, RNE, RTN, RTP, RTZ),
    ValidFP,
  )
import Grisette.Internal.SymPrim.Prim.Internal.Term
  ( FPBinaryOp (FPMaximum, FPMaximumNumber, FPMinimum, FPMinimumNumber, FPRem),
    FPRoundingBinaryOp (FPAdd, FPDiv, FPMul, FPSub),
    FPRoundingUnaryOp (FPRoundToIntegral, FPSqrt),
    FPUnaryOp (FPAbs, FPNeg),
    FloatingUnaryOp (FloatingSqrt),
    PEvalBitCastOrTerm (pevalBitCastOrTerm),
    PEvalBitCastTerm (pevalBitCastTerm),
    PEvalFloatingTerm (pevalFloatingUnaryTerm, pevalPowerTerm),
    PEvalFractionalTerm (pevalRecipTerm),
    PEvalIEEEFPConvertibleTerm (pevalFromFPOrTerm, pevalToFPTerm),
    SupportedPrim (pevalDistinctTerm),
    bitCastOrTerm,
    bitCastTerm,
    fdivTerm,
    floatingUnaryTerm,
    fpBinaryTerm,
    fpFMATerm,
    fpRoundingBinaryTerm,
    fpRoundingUnaryTerm,
    fpUnaryTerm,
    fromFPOrTerm,
    powerTerm,
    toFPTerm,
  )
import Grisette.Internal.SymPrim.Prim.Term
  ( FPTrait
      ( FPIsInfinite,
        FPIsNaN,
        FPIsNegative,
        FPIsNegativeInfinite,
        FPIsNegativeZero,
        FPIsNormal,
        FPIsPoint,
        FPIsPositive,
        FPIsPositiveInfinite,
        FPIsPositiveZero,
        FPIsSubnormal,
        FPIsZero
      ),
    PEvalBVTerm
      ( pevalBVConcatTerm,
        pevalBVExtendTerm,
        pevalBVSelectTerm
      ),
    PEvalBitwiseTerm
      ( pevalAndBitsTerm,
        pevalComplementBitsTerm,
        pevalOrBitsTerm,
        pevalXorBitsTerm
      ),
    PEvalDivModIntegralTerm
      ( pevalDivIntegralTerm,
        pevalModIntegralTerm,
        pevalQuotIntegralTerm,
        pevalRemIntegralTerm
      ),
    PEvalFractionalTerm (pevalFdivTerm),
    PEvalNumTerm
      ( pevalAbsNumTerm,
        pevalAddNumTerm,
        pevalMulNumTerm,
        pevalNegNumTerm,
        pevalSignumNumTerm
      ),
    PEvalOrdTerm (pevalLeOrdTerm, pevalLtOrdTerm),
    PEvalRotateTerm
      ( pevalRotateLeftTerm,
        pevalRotateRightTerm
      ),
    PEvalShiftTerm
      ( pevalShiftLeftTerm,
        pevalShiftRightTerm
      ),
    SupportedNonFuncPrim,
    SupportedPrim (pevalITETerm),
    Term,
    absNumTerm,
    addNumTerm,
    andBitsTerm,
    andTerm,
    bvConcatTerm,
    bvExtendTerm,
    bvSelectTerm,
    complementBitsTerm,
    conTerm,
    distinctTerm,
    divIntegralTerm,
    eqTerm,
    fpTraitTerm,
    iteTerm,
    leOrdTerm,
    ltOrdTerm,
    modIntegralTerm,
    mulNumTerm,
    negNumTerm,
    notTerm,
    orBitsTerm,
    orTerm,
    pevalAndTerm,
    pevalEqTerm,
    pevalFPBinaryTerm,
    pevalFPFMATerm,
    pevalFPRoundingBinaryTerm,
    pevalFPRoundingUnaryTerm,
    pevalFPTraitTerm,
    pevalFPUnaryTerm,
    pevalNotTerm,
    pevalOrTerm,
    pformatTerm,
    quotIntegralTerm,
    recipTerm,
    remIntegralTerm,
    rotateLeftTerm,
    rotateRightTerm,
    shiftLeftTerm,
    shiftRightTerm,
    signumNumTerm,
    ssymTerm,
    xorBitsTerm,
  )
import Test.QuickCheck (Arbitrary (arbitrary), Gen, elements, frequency, oneof, sized)

-- import Grisette.Internal.SymPrim.FP (FPRoundingMode(RNE))

class (SupportedNonFuncPrim b) => TermRewritingSpec a b | a -> b where
  norewriteVer :: a -> Term b
  rewriteVer :: a -> Term b
  wrap :: Term b -> Term b -> a
  same :: a -> Term Bool
  counterExample :: a -> Term Bool
  counterExample = notTerm . same
  symSpec :: Identifier -> a
  symSpec s = wrap (ssymTerm s) (ssymTerm s)
  conSpec :: b -> a
  conSpec v = wrap (conTerm v) (conTerm v)

constructUnarySpec ::
  forall a av b bv.
  ( TermRewritingSpec a av,
    TermRewritingSpec b bv
  ) =>
  (Term av -> Term bv) ->
  (Term av -> Term bv) ->
  a ->
  b
constructUnarySpec construct partial a =
  wrap (construct $ norewriteVer a) (partial $ rewriteVer a)

constructBinarySpec ::
  forall a av b bv c cv.
  ( TermRewritingSpec a av,
    TermRewritingSpec b bv,
    TermRewritingSpec c cv
  ) =>
  (Term av -> Term bv -> Term cv) ->
  (Term av -> Term bv -> Term cv) ->
  a ->
  b ->
  c
constructBinarySpec construct partial a b =
  wrap
    (construct (norewriteVer a) (norewriteVer b))
    (partial (rewriteVer a) (rewriteVer b))

constructTernarySpec ::
  forall a av b bv c cv d dv.
  ( TermRewritingSpec a av,
    TermRewritingSpec b bv,
    TermRewritingSpec c cv,
    TermRewritingSpec d dv
  ) =>
  (Term av -> Term bv -> Term cv -> Term dv) ->
  (Term av -> Term bv -> Term cv -> Term dv) ->
  a ->
  b ->
  c ->
  d
constructTernarySpec construct partial a b c =
  wrap
    (construct (norewriteVer a) (norewriteVer b) (norewriteVer c))
    (partial (rewriteVer a) (rewriteVer b) (rewriteVer c))

notSpec :: (TermRewritingSpec a Bool) => a -> a
notSpec = constructUnarySpec notTerm pevalNotTerm

andSpec :: (TermRewritingSpec a Bool) => a -> a -> a
andSpec = constructBinarySpec andTerm pevalAndTerm

orSpec :: (TermRewritingSpec a Bool) => a -> a -> a
orSpec = constructBinarySpec orTerm pevalOrTerm

eqvSpec :: (TermRewritingSpec a av, TermRewritingSpec b Bool) => a -> a -> b
eqvSpec = constructBinarySpec eqTerm pevalEqTerm

distinctSpec ::
  (TermRewritingSpec a av, TermRewritingSpec b Bool) => NonEmpty a -> b
distinctSpec l =
  wrap
    (distinctTerm (norewriteVer <$> l))
    (pevalDistinctTerm (rewriteVer <$> l))

iteSpec :: (TermRewritingSpec a Bool, TermRewritingSpec b bv) => a -> b -> b -> b
iteSpec = constructTernarySpec iteTerm pevalITETerm

addNumSpec :: (TermRewritingSpec a av, PEvalNumTerm av) => a -> a -> a
addNumSpec = constructBinarySpec addNumTerm pevalAddNumTerm

negNumSpec :: (TermRewritingSpec a av, PEvalNumTerm av) => a -> a
negNumSpec = constructUnarySpec negNumTerm pevalNegNumTerm

mulNumSpec :: (TermRewritingSpec a av, PEvalNumTerm av) => a -> a -> a
mulNumSpec = constructBinarySpec mulNumTerm pevalMulNumTerm

absNumSpec :: (TermRewritingSpec a av, PEvalNumTerm av) => a -> a
absNumSpec = constructUnarySpec absNumTerm pevalAbsNumTerm

signumNumSpec :: (TermRewritingSpec a av, PEvalNumTerm av) => a -> a
signumNumSpec = constructUnarySpec signumNumTerm pevalSignumNumTerm

bitCastSpec ::
  ( TermRewritingSpec a av,
    TermRewritingSpec b bv,
    PEvalBitCastTerm av bv
  ) =>
  a ->
  b
bitCastSpec = constructUnarySpec bitCastTerm pevalBitCastTerm

bitCastOrSpec ::
  ( TermRewritingSpec a av,
    TermRewritingSpec b bv,
    PEvalBitCastOrTerm av bv
  ) =>
  b ->
  a ->
  b
bitCastOrSpec = constructBinarySpec bitCastOrTerm pevalBitCastOrTerm

ltOrdSpec ::
  (TermRewritingSpec a av, PEvalOrdTerm av, TermRewritingSpec b Bool) =>
  a ->
  a ->
  b
ltOrdSpec = constructBinarySpec ltOrdTerm pevalLtOrdTerm

leOrdSpec ::
  (TermRewritingSpec a av, PEvalOrdTerm av, TermRewritingSpec b Bool) =>
  a ->
  a ->
  b
leOrdSpec = constructBinarySpec leOrdTerm pevalLeOrdTerm

andBitsSpec :: (TermRewritingSpec a av, PEvalBitwiseTerm av) => a -> a -> a
andBitsSpec = constructBinarySpec andBitsTerm pevalAndBitsTerm

orBitsSpec :: (TermRewritingSpec a av, PEvalBitwiseTerm av) => a -> a -> a
orBitsSpec = constructBinarySpec orBitsTerm pevalOrBitsTerm

xorBitsSpec :: (TermRewritingSpec a av, PEvalBitwiseTerm av) => a -> a -> a
xorBitsSpec = constructBinarySpec xorBitsTerm pevalXorBitsTerm

complementBitsSpec :: (TermRewritingSpec a av, PEvalBitwiseTerm av) => a -> a
complementBitsSpec = constructUnarySpec complementBitsTerm pevalComplementBitsTerm

shiftLeftSpec :: (TermRewritingSpec a av, PEvalShiftTerm av) => a -> a -> a
shiftLeftSpec = constructBinarySpec shiftLeftTerm pevalShiftLeftTerm

shiftRightSpec :: (TermRewritingSpec a av, PEvalShiftTerm av) => a -> a -> a
shiftRightSpec = constructBinarySpec shiftRightTerm pevalShiftRightTerm

rotateLeftSpec :: (TermRewritingSpec a av, PEvalRotateTerm av) => a -> a -> a
rotateLeftSpec = constructBinarySpec rotateLeftTerm pevalRotateLeftTerm

rotateRightSpec :: (TermRewritingSpec a av, PEvalRotateTerm av) => a -> a -> a
rotateRightSpec = constructBinarySpec rotateRightTerm pevalRotateRightTerm

bvconcatSpec ::
  ( TermRewritingSpec a (bv an),
    TermRewritingSpec b (bv bn),
    TermRewritingSpec c (bv (an + bn)),
    PEvalBVTerm bv,
    KnownNat an,
    KnownNat bn,
    KnownNat (an + bn),
    1 <= an,
    1 <= bn,
    1 <= an + bn
  ) =>
  a ->
  b ->
  c
bvconcatSpec = constructBinarySpec bvConcatTerm pevalBVConcatTerm

bvselectSpec ::
  ( TermRewritingSpec a (bv an),
    TermRewritingSpec b (bv bn),
    PEvalBVTerm bv,
    KnownNat an,
    KnownNat ix,
    KnownNat bn,
    1 <= an,
    1 <= bn,
    ix + bn <= an
  ) =>
  proxy ix ->
  proxy bn ->
  a ->
  b
bvselectSpec p1 p2 = constructUnarySpec (bvSelectTerm p1 p2) (pevalBVSelectTerm p1 p2)

bvextendSpec ::
  ( TermRewritingSpec a (bv an),
    TermRewritingSpec b (bv bn),
    PEvalBVTerm bv,
    KnownNat an,
    KnownNat bn,
    1 <= an,
    1 <= bn,
    an <= bn
  ) =>
  Bool ->
  proxy bn ->
  a ->
  b
bvextendSpec signed p = constructUnarySpec (bvExtendTerm signed p) (pevalBVExtendTerm signed p)

divIntegralSpec :: (TermRewritingSpec a b, PEvalDivModIntegralTerm b) => a -> a -> a
divIntegralSpec = constructBinarySpec divIntegralTerm pevalDivIntegralTerm

modIntegralSpec :: (TermRewritingSpec a b, PEvalDivModIntegralTerm b) => a -> a -> a
modIntegralSpec = constructBinarySpec modIntegralTerm pevalModIntegralTerm

quotIntegralSpec :: (TermRewritingSpec a b, PEvalDivModIntegralTerm b) => a -> a -> a
quotIntegralSpec = constructBinarySpec quotIntegralTerm pevalQuotIntegralTerm

remIntegralSpec :: (TermRewritingSpec a b, PEvalDivModIntegralTerm b) => a -> a -> a
remIntegralSpec = constructBinarySpec remIntegralTerm pevalRemIntegralTerm

fpTraitSpec ::
  ( ValidFP eb fb,
    TermRewritingSpec a (FP eb fb),
    TermRewritingSpec b Bool
  ) =>
  FPTrait ->
  a ->
  b
fpTraitSpec trait = constructUnarySpec (fpTraitTerm trait) (pevalFPTraitTerm trait)

fdivSpec :: (TermRewritingSpec a av, PEvalFractionalTerm av) => a -> a -> a
fdivSpec = constructBinarySpec fdivTerm pevalFdivTerm

recipSpec :: (TermRewritingSpec a av, PEvalFractionalTerm av) => a -> a
recipSpec = constructUnarySpec recipTerm pevalRecipTerm

floatingUnarySpec ::
  (TermRewritingSpec a av, PEvalFloatingTerm av) => FloatingUnaryOp -> a -> a
floatingUnarySpec op =
  constructUnarySpec
    (floatingUnaryTerm op)
    (pevalFloatingUnaryTerm op)

powerSpec ::
  (TermRewritingSpec a av, PEvalFloatingTerm av) => a -> a -> a
powerSpec = constructBinarySpec powerTerm pevalPowerTerm

fpUnaryOpSpec ::
  ( ValidFP eb fb,
    TermRewritingSpec a (FP eb fb)
  ) =>
  FPUnaryOp ->
  a ->
  a
fpUnaryOpSpec op = constructUnarySpec (fpUnaryTerm op) (pevalFPUnaryTerm op)

fpBinaryOpSpec ::
  ( ValidFP eb fb,
    TermRewritingSpec a (FP eb fb)
  ) =>
  FPBinaryOp ->
  a ->
  a ->
  a
fpBinaryOpSpec op = constructBinarySpec (fpBinaryTerm op) (pevalFPBinaryTerm op)

fpRoundingUnaryOpSpec ::
  ( ValidFP eb fb,
    TermRewritingSpec a (FP eb fb),
    TermRewritingSpec r FPRoundingMode
  ) =>
  FPRoundingUnaryOp ->
  r ->
  a ->
  a
fpRoundingUnaryOpSpec op =
  constructBinarySpec (fpRoundingUnaryTerm op) (pevalFPRoundingUnaryTerm op)

fpRoundingBinarySpec ::
  ( ValidFP eb fb,
    TermRewritingSpec a (FP eb fb),
    TermRewritingSpec r FPRoundingMode
  ) =>
  FPRoundingBinaryOp ->
  r ->
  a ->
  a ->
  a
fpRoundingBinarySpec op =
  constructTernarySpec (fpRoundingBinaryTerm op) (pevalFPRoundingBinaryTerm op)

fpFMASpec ::
  ( ValidFP eb fb,
    TermRewritingSpec a (FP eb fb),
    TermRewritingSpec r FPRoundingMode
  ) =>
  r ->
  a ->
  a ->
  a ->
  a
fpFMASpec a b c d =
  wrap
    (fpFMATerm (norewriteVer a) (norewriteVer b) (norewriteVer c) (norewriteVer d))
    (pevalFPFMATerm (rewriteVer a) (rewriteVer b) (rewriteVer c) (norewriteVer d))

fromFPOrSpec ::
  ( ValidFP eb fb,
    TermRewritingSpec a (FP eb fb),
    TermRewritingSpec rd FPRoundingMode,
    TermRewritingSpec b i,
    PEvalIEEEFPConvertibleTerm i
  ) =>
  b ->
  rd ->
  a ->
  b
fromFPOrSpec = constructTernarySpec fromFPOrTerm pevalFromFPOrTerm

toFPSpec ::
  ( ValidFP eb fb,
    TermRewritingSpec a (FP eb fb),
    TermRewritingSpec rd FPRoundingMode,
    TermRewritingSpec b i,
    PEvalIEEEFPConvertibleTerm i
  ) =>
  rd ->
  b ->
  a
toFPSpec = constructBinarySpec toFPTerm pevalToFPTerm

data BoolOnlySpec = BoolOnlySpec (Term Bool) (Term Bool)

instance Show BoolOnlySpec where
  show (BoolOnlySpec n r) = "BoolOnlySpec { no: " ++ pformatTerm n ++ ", re: " ++ pformatTerm r ++ " }"

instance TermRewritingSpec BoolOnlySpec Bool where
  norewriteVer (BoolOnlySpec n _) = n
  rewriteVer (BoolOnlySpec _ r) = r
  wrap = BoolOnlySpec
  same s = eqTerm (norewriteVer s) (rewriteVer s)

boolonly :: Int -> Gen BoolOnlySpec
boolonly 0 =
  let s =
        oneof $
          return . symSpec . (`withMetadata` (Atom ("bool" :: T.Text)))
            <$> ["a", "b", "c", "d", "e", "f", "g"]
      r = oneof $ return . conSpec <$> [True, False]
   in oneof [r, s]
boolonly n | n > 0 = do
  v1 <- boolonly (n - 1)
  v2 <- boolonly (n - 1)
  v3 <- boolonly (n - 1)
  oneof
    [ return $ notSpec v1,
      return $ andSpec v1 v2,
      return $ orSpec v1 v2,
      return $ eqvSpec v1 v2,
      return $ distinctSpec $ v1 :| [],
      return $ distinctSpec $ v1 :| [v2],
      return $ distinctSpec $ v1 :| [v2, v3],
      return $ iteSpec v1 v2 v3
    ]
boolonly _ = error "Should never be called"

instance Arbitrary BoolOnlySpec where
  arbitrary = sized boolonly

data BoolWithLIASpec = BoolWithLIASpec (Term Bool) (Term Bool)

instance Show BoolWithLIASpec where
  show (BoolWithLIASpec n r) = "BoolWithLIASpec { no: " ++ pformatTerm n ++ ", re: " ++ pformatTerm r ++ " }"

instance TermRewritingSpec BoolWithLIASpec Bool where
  norewriteVer (BoolWithLIASpec n _) = n
  rewriteVer (BoolWithLIASpec _ r) = r
  wrap = BoolWithLIASpec
  same s = eqTerm (norewriteVer s) (rewriteVer s)

data LIAWithBoolSpec = LIAWithBoolSpec (Term Integer) (Term Integer)

instance Show LIAWithBoolSpec where
  show (LIAWithBoolSpec n r) =
    "LIAWithBoolSpec { no: " ++ pformatTerm n ++ ", re: " ++ pformatTerm r ++ " }"

instance TermRewritingSpec LIAWithBoolSpec Integer where
  norewriteVer (LIAWithBoolSpec n _) = n
  rewriteVer (LIAWithBoolSpec _ r) = r
  wrap = LIAWithBoolSpec
  same s = eqTerm (norewriteVer s) (rewriteVer s)

boolWithLIA :: Int -> Gen BoolWithLIASpec
boolWithLIA 0 =
  let s =
        oneof $
          return . symSpec . (`withMetadata` (Atom ("bool" :: T.Text)))
            <$> ["a", "b", "c", "d", "e", "f", "g"]
      r = oneof $ return . conSpec <$> [True, False]
   in oneof [r, s]
boolWithLIA n | n > 0 = do
  v1 <- boolWithLIA (n - 1)
  v2 <- boolWithLIA (n - 1)
  v3 <- boolWithLIA (n - 1)
  v1i <- liaWithBool (n - 1)
  v2i <- liaWithBool (n - 1)
  v3i <- liaWithBool (n - 1)
  frequency
    [ (1, return $ notSpec v1),
      (1, return $ andSpec v1 v2),
      (1, return $ orSpec v1 v2),
      (1, return $ eqvSpec v1 v2),
      (4, return $ eqvSpec v1i v2i),
      (1, return $ distinctSpec $ v1 :| []),
      (1, return $ distinctSpec $ v1 :| [v2]),
      (1, return $ distinctSpec $ v1 :| [v2, v3]),
      (2, return $ distinctSpec $ v1i :| []),
      (2, return $ distinctSpec $ v1i :| [v2i]),
      (2, return $ distinctSpec $ v1i :| [v2i, v3i]),
      (5, return $ ltOrdSpec v1i v2i),
      (5, return $ leOrdSpec v1i v2i),
      (1, return $ iteSpec v1 v2 v3)
    ]
boolWithLIA _ = error "Should never be called"

liaWithBool :: Int -> Gen LIAWithBoolSpec
liaWithBool 0 =
  let s =
        oneof $
          return . symSpec . (`withMetadata` (Atom ("int" :: T.Text)))
            <$> ["a", "b", "c", "d", "e", "f", "g"]
      r = conSpec <$> arbitrary
   in oneof [r, s]
liaWithBool n | n > 0 = do
  v1b <- boolWithLIA (n - 1)
  v1i <- liaWithBool (n - 1)
  v2i <- liaWithBool (n - 1)
  oneof
    [ return $ negNumSpec v1i,
      return $ absNumSpec v1i,
      return $ signumNumSpec v1i,
      return $ addNumSpec v1i v2i,
      return $ iteSpec v1b v1i v2i
    ]
liaWithBool _ = error "Should never be called"

instance Arbitrary BoolWithLIASpec where
  arbitrary = sized boolWithLIA

instance Arbitrary LIAWithBoolSpec where
  arbitrary = sized liaWithBool

data FixedSizedBVWithBoolSpec (bv :: Nat -> Type) (n :: Nat) = FixedSizedBVWithBoolSpec (Term (bv n)) (Term (bv n))

instance (SupportedNonFuncPrim (bv n)) => Show (FixedSizedBVWithBoolSpec bv n) where
  show (FixedSizedBVWithBoolSpec n r) = "FixedSizedBVWithBoolSpec { no: " ++ pformatTerm n ++ ", re: " ++ pformatTerm r ++ " }"

instance (SupportedNonFuncPrim (bv n)) => TermRewritingSpec (FixedSizedBVWithBoolSpec bv n) (bv n) where
  norewriteVer (FixedSizedBVWithBoolSpec n _) = n
  rewriteVer (FixedSizedBVWithBoolSpec _ r) = r
  wrap = FixedSizedBVWithBoolSpec
  same s = eqTerm (norewriteVer s) (rewriteVer s)

data BoolWithFixedSizedBVSpec (bv :: Nat -> Type) (n :: Nat) = BoolWithFixedSizedBVSpec (Term Bool) (Term Bool)

instance Show (BoolWithFixedSizedBVSpec bv n) where
  show (BoolWithFixedSizedBVSpec n r) =
    "BoolWithFixedSizedBVSpec { no: " ++ pformatTerm n ++ ", re: " ++ pformatTerm r ++ " }"

instance TermRewritingSpec (BoolWithFixedSizedBVSpec bv n) Bool where
  norewriteVer (BoolWithFixedSizedBVSpec n _) = n
  rewriteVer (BoolWithFixedSizedBVSpec _ r) = r
  wrap = BoolWithFixedSizedBVSpec
  same s = eqTerm (norewriteVer s) (rewriteVer s)

boolWithFSBV ::
  forall p1 p2 bv n.
  (SupportedBV bv n) =>
  p1 bv ->
  p2 n ->
  Int ->
  Gen (BoolWithFixedSizedBVSpec bv n)
boolWithFSBV _ _ 0 =
  let s =
        oneof $
          return . symSpec . (`withMetadata` (Atom ("bool" :: T.Text)))
            <$> ["a", "b", "c", "d", "e", "f", "g"]
      r = oneof $ return . conSpec <$> [True, False]
   in oneof [r, s]
boolWithFSBV pbv pn n | n > 0 = do
  v1 <- boolWithFSBV pbv pn (n - 1)
  v2 <- boolWithFSBV pbv pn (n - 1)
  v3 <- boolWithFSBV pbv pn (n - 1)
  v1i <- fsbvWithBool pbv pn (n - 1)
  v2i <- fsbvWithBool pbv pn (n - 1)
  v3i <- fsbvWithBool pbv pn (n - 1)
  frequency
    [ (1, return $ notSpec v1),
      (1, return $ andSpec v1 v2),
      (1, return $ orSpec v1 v2),
      (1, return $ eqvSpec v1 v2),
      (5, return $ eqvSpec v1i v2i),
      (1, return $ distinctSpec $ v1 :| []),
      (1, return $ distinctSpec $ v1 :| [v2]),
      (1, return $ distinctSpec $ v1 :| [v2, v3]),
      (2, return $ distinctSpec $ v1i :| []),
      (2, return $ distinctSpec $ v1i :| [v2i]),
      (2, return $ distinctSpec $ v1i :| [v2i, v3i]),
      (5, return $ ltOrdSpec v1i v2i),
      (5, return $ leOrdSpec v1i v2i),
      (1, return $ iteSpec v1 v2 v3)
    ]
boolWithFSBV _ _ _ = error "Should never be called"

fsbvWithBool ::
  forall p1 p2 bv n.
  (SupportedBV bv n) =>
  p1 bv ->
  p2 n ->
  Int ->
  Gen (FixedSizedBVWithBoolSpec bv n)
fsbvWithBool _ _ 0 =
  let s =
        oneof $
          return . symSpec . (`withMetadata` (Atom ("int" :: T.Text)))
            <$> ["a", "b", "c", "d", "e", "f", "g"]
      r =
        conSpec
          <$> oneof
            [ return minBound,
              return maxBound,
              fromInteger <$> arbitrary
            ]
   in oneof [r, s]
fsbvWithBool pbv pn n | n > 0 = do
  v1b <- boolWithFSBV pbv pn (n - 1)
  v1i <- fsbvWithBool pbv pn (n - 1)
  v2i <- fsbvWithBool pbv pn (n - 1)
  oneof
    [ return $ negNumSpec v1i,
      return $ absNumSpec v1i,
      return $ signumNumSpec v1i,
      return $ addNumSpec v1i v2i,
      return $ mulNumSpec v1i v2i,
      return $ andBitsSpec v1i v2i,
      return $ orBitsSpec v1i v2i,
      return $ xorBitsSpec v1i v2i,
      return $ complementBitsSpec v1i,
      return $ shiftLeftSpec v1i v2i,
      return $ rotateLeftSpec v1i v2i,
      return $ shiftRightSpec v1i v2i,
      return $ rotateRightSpec v1i v2i,
      return $ iteSpec v1b v1i v2i
    ]
fsbvWithBool _ _ _ = error "Should never be called"

instance (SupportedBV bv n) => Arbitrary (BoolWithFixedSizedBVSpec bv n) where
  arbitrary = sized (boolWithFSBV (Proxy @bv) (Proxy @n))

instance (SupportedBV bv n) => Arbitrary (FixedSizedBVWithBoolSpec bv n) where
  arbitrary = sized (fsbvWithBool Proxy Proxy)

data DifferentSizeBVSpec bv (n :: Nat) = DifferentSizeBVSpec (Term (bv n)) (Term (bv n))

instance (SupportedNonFuncPrim (bv n)) => Show (DifferentSizeBVSpec bv n) where
  show (DifferentSizeBVSpec n r) = "DSizeBVSpec { no: " ++ pformatTerm n ++ ", re: " ++ pformatTerm r ++ " }"

instance (SupportedNonFuncPrim (bv n)) => TermRewritingSpec (DifferentSizeBVSpec bv n) (bv n) where
  norewriteVer (DifferentSizeBVSpec n _) = n
  rewriteVer (DifferentSizeBVSpec _ r) = r
  wrap = DifferentSizeBVSpec
  same s = eqTerm (norewriteVer s) (rewriteVer s)

type SupportedBV bv (n :: Nat) =
  ( SupportedPrim (bv n),
    SupportedNonFuncPrim (bv n),
    Ord (bv n),
    Num (bv n),
    FiniteBits (bv n),
    Integral (bv n),
    Bounded (bv n),
    SymShift (bv n),
    SymRotate (bv n),
    PEvalShiftTerm (bv n),
    PEvalRotateTerm (bv n),
    PEvalNumTerm (bv n),
    PEvalOrdTerm (bv n),
    PEvalBitwiseTerm (bv n),
    KnownNat n,
    PEvalBVTerm bv,
    1 <= n
  )

dsbv1 ::
  forall proxy bv.
  ( SupportedBV bv 1,
    SupportedBV bv 2,
    SupportedBV bv 3,
    SupportedBV bv 4
  ) =>
  proxy bv ->
  Int ->
  Gen (DifferentSizeBVSpec bv 1)
dsbv1 _ 0 =
  let s =
        oneof $
          return . symSpec . (`withMetadata` (Atom ("bv1" :: T.Text)))
            <$> ["a", "b", "c", "d", "e", "f", "g"]
      r = conSpec . fromInteger <$> arbitrary
   in oneof [r, s]
dsbv1 p depth | depth > 0 = do
  v1 <- dsbv1 p (depth - 1)
  v1' <- dsbv1 p (depth - 1)
  v2 <- dsbv2 p (depth - 1)
  v3 <- dsbv3 p (depth - 1)
  v4 <- dsbv4 p (depth - 1)
  oneof
    [ return $ negNumSpec v1,
      return $ absNumSpec v1,
      return $ signumNumSpec v1,
      return $ addNumSpec v1 v1',
      return $ mulNumSpec v1 v1',
      return $ andBitsSpec v1 v1',
      return $ orBitsSpec v1 v1',
      return $ xorBitsSpec v1 v1',
      return $ complementBitsSpec v1,
      return $ shiftLeftSpec v1 v1',
      return $ rotateLeftSpec v1 v1',
      return $ shiftRightSpec v1 v1',
      return $ rotateRightSpec v1 v1',
      return $ bvselectSpec (Proxy @0) (Proxy @1) v4,
      return $ bvselectSpec (Proxy @1) (Proxy @1) v4,
      return $ bvselectSpec (Proxy @2) (Proxy @1) v4,
      return $ bvselectSpec (Proxy @3) (Proxy @1) v4,
      return $ bvselectSpec (Proxy @0) (Proxy @1) v3,
      return $ bvselectSpec (Proxy @1) (Proxy @1) v3,
      return $ bvselectSpec (Proxy @2) (Proxy @1) v3,
      return $ bvselectSpec (Proxy @0) (Proxy @1) v2,
      return $ bvselectSpec (Proxy @1) (Proxy @1) v2,
      return $ bvselectSpec (Proxy @0) (Proxy @1) v1
    ]
dsbv1 _ _ = error "Should never be called"

dsbv2 ::
  forall proxy bv.
  ( SupportedBV bv 1,
    SupportedBV bv 2,
    SupportedBV bv 3,
    SupportedBV bv 4
  ) =>
  proxy bv ->
  Int ->
  Gen (DifferentSizeBVSpec bv 2)
dsbv2 _ 0 =
  let s =
        oneof $
          return . symSpec . (`withMetadata` (Atom ("bv2" :: T.Text)))
            <$> ["a", "b", "c", "d", "e", "f", "g"]
      r = conSpec . fromInteger <$> arbitrary
   in oneof [r, s]
dsbv2 p depth | depth > 0 = do
  v1 <- dsbv1 p (depth - 1)
  v1' <- dsbv1 p (depth - 1)
  v2 <- dsbv2 p (depth - 1)
  v2' <- dsbv2 p (depth - 1)
  v3 <- dsbv3 p (depth - 1)
  v4 <- dsbv4 p (depth - 1)
  oneof
    [ return $ negNumSpec v2,
      return $ absNumSpec v2,
      return $ signumNumSpec v2,
      return $ addNumSpec v2 v2',
      return $ mulNumSpec v2 v2',
      return $ andBitsSpec v2 v2',
      return $ orBitsSpec v2 v2',
      return $ xorBitsSpec v2 v2',
      return $ complementBitsSpec v2,
      return $ shiftLeftSpec v2 v2',
      return $ rotateLeftSpec v2 v2',
      return $ shiftRightSpec v2 v2',
      return $ rotateRightSpec v2 v2',
      return $ bvselectSpec (Proxy @0) (Proxy @2) v4,
      return $ bvselectSpec (Proxy @1) (Proxy @2) v4,
      return $ bvselectSpec (Proxy @2) (Proxy @2) v4,
      return $ bvselectSpec (Proxy @0) (Proxy @2) v3,
      return $ bvselectSpec (Proxy @1) (Proxy @2) v3,
      return $ bvselectSpec (Proxy @0) (Proxy @2) v2,
      return $ bvconcatSpec v1 v1',
      return $ bvextendSpec False (Proxy @2) v1,
      return $ bvextendSpec True (Proxy @2) v1
    ]
dsbv2 _ _ = error "Should never be called"

dsbv3 ::
  forall proxy bv.
  ( SupportedBV bv 1,
    SupportedBV bv 2,
    SupportedBV bv 3,
    SupportedBV bv 4
  ) =>
  proxy bv ->
  Int ->
  Gen (DifferentSizeBVSpec bv 3)
dsbv3 _ 0 =
  let s =
        oneof $
          return . symSpec . (`withMetadata` (Atom ("bv3" :: T.Text)))
            <$> ["a", "b", "c", "d", "e", "f", "g"]
      r = conSpec . fromInteger <$> arbitrary
   in oneof [r, s]
dsbv3 p depth | depth > 0 = do
  v1 <- dsbv1 p (depth - 1)
  v2 <- dsbv2 p (depth - 1)
  v3 <- dsbv3 p (depth - 1)
  v3' <- dsbv3 p (depth - 1)
  v4 <- dsbv4 p (depth - 1)
  oneof
    [ return $ negNumSpec v3,
      return $ absNumSpec v3,
      return $ signumNumSpec v3,
      return $ addNumSpec v3 v3',
      return $ mulNumSpec v3 v3',
      return $ andBitsSpec v3 v3',
      return $ orBitsSpec v3 v3',
      return $ xorBitsSpec v3 v3',
      return $ complementBitsSpec v3,
      return $ shiftLeftSpec v3 v3',
      return $ rotateLeftSpec v3 v3',
      return $ shiftRightSpec v3 v3',
      return $ rotateRightSpec v3 v3',
      return $ bvselectSpec (Proxy @0) (Proxy @3) v4,
      return $ bvselectSpec (Proxy @1) (Proxy @3) v4,
      return $ bvselectSpec (Proxy @0) (Proxy @3) v3,
      return $ bvconcatSpec v1 v2,
      return $ bvconcatSpec v2 v1,
      return $ bvextendSpec False (Proxy @3) v1,
      return $ bvextendSpec True (Proxy @3) v1,
      return $ bvextendSpec False (Proxy @3) v2,
      return $ bvextendSpec True (Proxy @3) v2
    ]
dsbv3 _ _ = error "Should never be called"

dsbv4 ::
  forall proxy bv.
  ( SupportedBV bv 1,
    SupportedBV bv 2,
    SupportedBV bv 3,
    SupportedBV bv 4
  ) =>
  proxy bv ->
  Int ->
  Gen (DifferentSizeBVSpec bv 4)
dsbv4 _ 0 =
  let s =
        oneof $
          return . symSpec . (`withMetadata` (Atom ("bv4" :: T.Text)))
            <$> ["a", "b", "c", "d", "e", "f", "g"]
      r = conSpec . fromInteger <$> arbitrary
   in oneof [r, s]
dsbv4 p depth | depth > 0 = do
  v1 <- dsbv1 p (depth - 1)
  v2 <- dsbv2 p (depth - 1)
  v2' <- dsbv2 p (depth - 1)
  v3 <- dsbv3 p (depth - 1)
  v4 <- dsbv4 p (depth - 1)
  v4' <- dsbv4 p (depth - 1)
  oneof
    [ return $ negNumSpec v4,
      return $ absNumSpec v4,
      return $ signumNumSpec v4,
      return $ addNumSpec v4 v4',
      return $ mulNumSpec v4 v4',
      return $ andBitsSpec v4 v4',
      return $ orBitsSpec v4 v4',
      return $ xorBitsSpec v4 v4',
      return $ complementBitsSpec v4,
      return $ shiftLeftSpec v4 v4',
      return $ rotateLeftSpec v4 v4',
      return $ shiftRightSpec v4 v4',
      return $ rotateRightSpec v4 v4',
      return $ bvselectSpec (Proxy @0) (Proxy @4) v4,
      return $ bvconcatSpec v1 v3,
      return $ bvconcatSpec v2 v2',
      return $ bvconcatSpec v3 v1,
      return $ bvextendSpec False (Proxy @4) v1,
      return $ bvextendSpec True (Proxy @4) v1,
      return $ bvextendSpec False (Proxy @4) v2,
      return $ bvextendSpec True (Proxy @4) v2,
      return $ bvextendSpec False (Proxy @4) v3,
      return $ bvextendSpec True (Proxy @4) v3
    ]
dsbv4 _ _ = error "Should never be called"

instance
  ( SupportedBV bv 1,
    SupportedBV bv 2,
    SupportedBV bv 3,
    SupportedBV bv 4,
    Typeable bv,
    SizedBV bv
  ) =>
  Arbitrary (DifferentSizeBVSpec bv 4)
  where
  arbitrary = sized (dsbv4 Proxy)

data GeneralSpec s = GeneralSpec (Term s) (Term s)

instance (SupportedPrim s) => Show (GeneralSpec s) where
  show (GeneralSpec n r) = "GeneralSpec { no: " ++ pformatTerm n ++ ", re: " ++ pformatTerm r ++ " }"

instance (SupportedNonFuncPrim s) => TermRewritingSpec (GeneralSpec s) s where
  norewriteVer (GeneralSpec n _) = n
  rewriteVer (GeneralSpec _ r) = r
  wrap = GeneralSpec
  same s = eqTerm (norewriteVer s) (rewriteVer s)

data IEEEFPSpec eb sb = IEEEFPSpec (Term (FP eb sb)) (Term (FP eb sb))

instance (ValidFP eb sb) => Show (IEEEFPSpec eb sb) where
  show (IEEEFPSpec n r) =
    "IEEEFPSpec { no: " ++ pformatTerm n ++ ", re: " ++ pformatTerm r ++ " }"

instance (ValidFP eb sb) => TermRewritingSpec (IEEEFPSpec eb sb) (FP eb sb) where
  norewriteVer (IEEEFPSpec n _) = n
  rewriteVer (IEEEFPSpec _ r) = r
  wrap = IEEEFPSpec
  same s =
    orTerm
      ( andTerm
          (fpTraitTerm FPIsNaN (norewriteVer s))
          (fpTraitTerm FPIsNaN (rewriteVer s))
      )
      (eqTerm (norewriteVer s) (rewriteVer s))

instance (ValidFP eb sb) => Arbitrary (IEEEFPSpec eb sb) where
  arbitrary = do
    bool :: BoolOnlySpec <-
      oneof [conSpec <$> arbitrary, return $ symSpec "bool"]
    rounding :: FPRoundingModeSpec <- arbitrary
    let gen =
          oneof
            [conSpec <$> arbitrary, return $ symSpec "a", return $ symSpec "b"]
    a <- gen
    b <- gen
    c <- gen
    let regular =
          [ a,
            iteSpec bool a b,
            addNumSpec a a,
            negNumSpec a,
            mulNumSpec a b,
            absNumSpec a,
            signumNumSpec a,
            fdivSpec a b,
            recipSpec a,
            floatingUnarySpec FloatingSqrt a
          ]
    let uop = fpUnaryOpSpec <$> [FPAbs, FPNeg] <*> return a
    let bop =
          fpBinaryOpSpec
            <$> [ FPRem,
                  FPMinimum,
                  FPMaximum,
                  FPMaximumNumber,
                  FPMinimumNumber
                ]
            <*> [a]
            <*> [b]
    let ruop =
          fpRoundingUnaryOpSpec
            <$> [FPSqrt, FPRoundToIntegral]
            <*> [rounding]
            <*> [a]
    let rbop =
          fpRoundingBinarySpec
            <$> [FPAdd, FPSub, FPMul, FPDiv]
            <*> [rounding]
            <*> [a]
            <*> [b]
    oneof $
      return
        <$> regular
          ++ uop
          ++ bop
          ++ ruop
          ++ rbop
          ++ [fpFMASpec rounding a b c]

data IEEEFPBoolOpSpec (eb :: Nat) (sb :: Nat)
  = IEEEFPBoolOpSpec (Term Bool) (Term Bool)

instance Show (IEEEFPBoolOpSpec eb sb) where
  show (IEEEFPBoolOpSpec n r) =
    "IEEEFPBoolOpSpec { no: " ++ pformatTerm n ++ ", re: " ++ pformatTerm r ++ " }"

instance TermRewritingSpec (IEEEFPBoolOpSpec eb sb) Bool where
  norewriteVer (IEEEFPBoolOpSpec n _) = n
  rewriteVer (IEEEFPBoolOpSpec _ r) = r
  wrap = IEEEFPBoolOpSpec
  same s = eqTerm (norewriteVer s) (rewriteVer s)

singleFPBoolOpSpecGen ::
  forall eb sb. (ValidFP eb sb) => Gen (IEEEFPBoolOpSpec eb sb)
singleFPBoolOpSpecGen = do
  s0 :: IEEEFPSpec eb sb <- arbitrary
  s1 :: IEEEFPSpec eb sb <- arbitrary
  s2 :: IEEEFPSpec eb sb <- arbitrary
  let traitGens =
        [ FPIsNaN,
          FPIsPositive,
          FPIsNegative,
          FPIsPositiveInfinite,
          FPIsNegativeInfinite,
          FPIsInfinite,
          FPIsPositiveZero,
          FPIsNegativeZero,
          FPIsZero,
          FPIsNormal,
          FPIsSubnormal,
          FPIsPoint
        ]
          <&> (\trait -> return $ fpTraitSpec trait s0)
  let cmpGens =
        return
          <$> [ eqvSpec s0 s1,
                distinctSpec $ s0 :| [],
                distinctSpec $ s0 :| [s1],
                distinctSpec $ s0 :| [s1, s2],
                ltOrdSpec s0 s1,
                leOrdSpec s0 s1
              ]
  oneof $ traitGens ++ cmpGens

instance (ValidFP eb sb) => Arbitrary (IEEEFPBoolOpSpec eb sb) where
  arbitrary = singleFPBoolOpSpecGen

data FPRoundingModeSpec
  = FPRoundingModeSpec (Term FPRoundingMode) (Term FPRoundingMode)

instance Show FPRoundingModeSpec where
  show (FPRoundingModeSpec n r) =
    "FPRoundingModeSpec { no: " ++ pformatTerm n ++ ", re: " ++ pformatTerm r ++ " }"

instance TermRewritingSpec FPRoundingModeSpec FPRoundingMode where
  norewriteVer (FPRoundingModeSpec n _) = n
  rewriteVer (FPRoundingModeSpec _ r) = r
  wrap = FPRoundingModeSpec
  same s = eqTerm (norewriteVer s) (rewriteVer s)

instance Arbitrary FPRoundingModeSpec where
  arbitrary =
    elements
      [ conSpec RNE,
        conSpec RNA,
        conSpec RTP,
        conSpec RTN,
        conSpec RTZ,
        symSpec "a",
        symSpec "b"
      ]

data FPRoundingModeBoolOpSpec = FPRoundingModeBoolOpSpec (Term Bool) (Term Bool)

instance Show FPRoundingModeBoolOpSpec where
  show (FPRoundingModeBoolOpSpec n r) =
    "FPRoundingModeBoolOpSpec { no: " ++ pformatTerm n ++ ", re: " ++ pformatTerm r ++ " }"

instance TermRewritingSpec FPRoundingModeBoolOpSpec Bool where
  norewriteVer (FPRoundingModeBoolOpSpec n _) = n
  rewriteVer (FPRoundingModeBoolOpSpec _ r) = r
  wrap = FPRoundingModeBoolOpSpec
  same s = eqTerm (norewriteVer s) (rewriteVer s)

instance Arbitrary FPRoundingModeBoolOpSpec where
  arbitrary = do
    l :: FPRoundingModeSpec <- arbitrary
    r <- arbitrary
    x <- arbitrary
    elements
      [ eqvSpec l r,
        distinctSpec $ l :| [],
        distinctSpec $ l :| [r],
        distinctSpec $ l :| [r, x],
        ltOrdSpec l r,
        leOrdSpec l r
      ]

data BoolWithNRASpec = BoolWithNRASpec (Term Bool) (Term Bool)

instance Show BoolWithNRASpec where
  show (BoolWithNRASpec n r) =
    "BoolWithNRASpec { no: " ++ pformatTerm n ++ ", re: " ++ pformatTerm r ++ " }"

instance TermRewritingSpec BoolWithNRASpec Bool where
  norewriteVer (BoolWithNRASpec n _) = n
  rewriteVer (BoolWithNRASpec _ r) = r
  wrap = BoolWithNRASpec
  same s = eqTerm (norewriteVer s) (rewriteVer s)

data NRAWithBoolSpec = NRAWithBoolSpec (Term AlgReal) (Term AlgReal)

instance Show NRAWithBoolSpec where
  show (NRAWithBoolSpec n r) =
    "NRAWithBoolSpec { no: " ++ pformatTerm n ++ ", re: " ++ pformatTerm r ++ " }"

instance TermRewritingSpec NRAWithBoolSpec AlgReal where
  norewriteVer (NRAWithBoolSpec n _) = n
  rewriteVer (NRAWithBoolSpec _ r) = r
  wrap = NRAWithBoolSpec
  same s = eqTerm (norewriteVer s) (rewriteVer s)

boolWithNRA :: Int -> Gen BoolWithNRASpec
boolWithNRA 0 =
  let s =
        oneof $
          return . symSpec . (`withMetadata` (Atom ("bool" :: T.Text)))
            <$> ["a", "b", "c", "d", "e", "f", "g"]
      r = oneof $ return . conSpec <$> [True, False]
   in oneof [r, s]
boolWithNRA n | n > 0 = do
  v1 <- boolWithNRA (n - 1)
  v2 <- boolWithNRA (n - 1)
  v3 <- boolWithNRA (n - 1)
  v1i <- nraWithBool (n - 1)
  v2i <- nraWithBool (n - 1)
  v3i <- nraWithBool (n - 1)
  frequency
    [ (1, return $ notSpec v1),
      (1, return $ andSpec v1 v2),
      (1, return $ orSpec v1 v2),
      (1, return $ eqvSpec v1 v2),
      (5, return $ eqvSpec v1i v2i),
      (1, return $ distinctSpec $ v1 :| []),
      (1, return $ distinctSpec $ v1 :| [v2]),
      (1, return $ distinctSpec $ v1 :| [v2, v3]),
      (2, return $ distinctSpec $ v1i :| []),
      (2, return $ distinctSpec $ v1i :| [v2i]),
      (2, return $ distinctSpec $ v1i :| [v2i, v3i]),
      (5, return $ ltOrdSpec v1i v2i),
      (5, return $ leOrdSpec v1i v2i),
      (1, return $ iteSpec v1 v2 v3)
    ]
boolWithNRA _ = error "Should never be called"

nraWithBool :: Int -> Gen NRAWithBoolSpec
nraWithBool 0 =
  let s =
        oneof $
          return . symSpec . (`withMetadata` (Atom ("real" :: T.Text)))
            <$> ["a", "b", "c", "d", "e", "f", "g"]
      r = conSpec <$> arbitrary
   in oneof [r, s]
nraWithBool n | n > 0 = do
  v1b <- boolWithNRA (n - 1)
  v1i <- nraWithBool (n - 1)
  v2i <- nraWithBool (n - 1)
  oneof
    [ return $ negNumSpec v1i,
      return $ absNumSpec v1i,
      return $ signumNumSpec v1i,
      return $ addNumSpec v1i v2i,
      return $ mulNumSpec v1i v2i,
      return $ fdivSpec v1i v2i,
      return $ recipSpec v1i,
      return $ iteSpec v1b v1i v2i
    ]
nraWithBool _ = error "Should never be called"

instance Arbitrary BoolWithNRASpec where
  arbitrary = sized boolWithNRA

instance Arbitrary NRAWithBoolSpec where
  arbitrary = sized nraWithBool
