{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.SomeBV
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.SomeBV
  ( SomeBV (..),
    SomeBVException (..),

    -- * Constructing and pattern matching on SomeBV
    unsafeSomeBV,
    conBV,
    conBVView,
    pattern ConBV,
    symBV,
    ssymBV,
    isymBV,
    arbitraryBV,

    -- * Synonyms
    pattern SomeIntN,
    type SomeIntN,
    pattern SomeWordN,
    type SomeWordN,
    pattern SomeSymIntN,
    type SomeSymIntN,
    pattern SomeSymWordN,
    type SomeSymWordN,

    -- * Helpers for manipulating SomeBV
    unarySomeBV,
    unarySomeBVR1,
    binSomeBV,
    binSomeBVR1,
    binSomeBVR2,
    binSomeBVSafe,
    binSomeBVSafeR1,
    binSomeBVSafeR2,
  )
where

import Control.DeepSeq (NFData (rnf))
import Control.Exception (Exception, throw)
import Control.Monad (when)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Data.Bifunctor (Bifunctor (bimap))
import qualified Data.Binary as Binary
import Data.Bits
  ( Bits
      ( bit,
        bitSize,
        bitSizeMaybe,
        clearBit,
        complement,
        complementBit,
        isSigned,
        popCount,
        rotate,
        rotateL,
        rotateR,
        setBit,
        shift,
        shiftL,
        shiftR,
        testBit,
        unsafeShiftL,
        unsafeShiftR,
        xor,
        zeroBits,
        (.&.),
        (.|.)
      ),
    FiniteBits (countLeadingZeros, countTrailingZeros, finiteBitSize),
  )
import Data.Bytes.Get (MonadGet (getWord8))
import Data.Bytes.Put (MonadPut (putWord8))
import Data.Bytes.Serial (Serial (deserialize, serialize))
import Data.Data (Proxy (Proxy))
import Data.Hashable (Hashable (hashWithSalt))
import Data.Maybe (catMaybes, fromJust, isJust)
import qualified Data.Serialize as Cereal
import qualified Data.Text as T
import Data.Type.Equality (type (:~:) (Refl))
import GHC.Exception (Exception (displayException))
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import GHC.TypeNats
  ( KnownNat,
    Nat,
    natVal,
    sameNat,
    type (+),
    type (<=),
  )
import Generics.Deriving (Default (Default))
import Grisette.Internal.Core.Control.Monad.Union (Union)
import Grisette.Internal.Core.Data.Class.BitVector
  ( BV (bv, bvConcat, bvExt, bvSelect, bvSext, bvZext),
    SizedBV
      ( sizedBVConcat,
        sizedBVExt,
        sizedBVFromIntegral,
        sizedBVSelect,
        sizedBVSext,
        sizedBVZext
      ),
  )
import Grisette.Internal.Core.Data.Class.EvalSym
  ( EvalSym (evalSym),
  )
import Grisette.Internal.Core.Data.Class.ExtractSym
  ( ExtractSym (extractSymMaybe),
  )
import Grisette.Internal.Core.Data.Class.GenSym
  ( GenSym (fresh),
    GenSymSimple (simpleFresh),
  )
import Grisette.Internal.Core.Data.Class.ITEOp (ITEOp (symIte))
import Grisette.Internal.Core.Data.Class.Mergeable
  ( Mergeable (rootStrategy),
    MergingStrategy (SimpleStrategy, SortedStrategy),
    wrapStrategy,
  )
import Grisette.Internal.Core.Data.Class.PPrint
  ( PPrint (pformat),
  )
import Grisette.Internal.Core.Data.Class.PlainUnion (simpleMerge)
import Grisette.Internal.Core.Data.Class.SafeDiv
  ( DivOr (divModOr, divOr, modOr, quotOr, quotRemOr, remOr),
    SafeDiv (safeDiv, safeDivMod, safeMod, safeQuot, safeQuotRem, safeRem),
  )
import Grisette.Internal.Core.Data.Class.SafeLinearArith
  ( SafeLinearArith (safeAdd, safeNeg, safeSub),
  )
import Grisette.Internal.Core.Data.Class.SafeSymRotate
  ( SafeSymRotate (safeSymRotateL, safeSymRotateR),
  )
import Grisette.Internal.Core.Data.Class.SafeSymShift
  ( SafeSymShift
      ( safeSymShiftL,
        safeSymShiftR,
        safeSymStrictShiftL,
        safeSymStrictShiftR
      ),
  )
import Grisette.Internal.Core.Data.Class.SignConversion
  ( SignConversion (toSigned, toUnsigned),
  )
import Grisette.Internal.Core.Data.Class.SimpleMergeable (mrgIf)
import Grisette.Internal.Core.Data.Class.Solvable
  ( Solvable (con, conView, isym, ssym, sym),
  )
import Grisette.Internal.Core.Data.Class.SubstSym
  ( SubstSym (substSym),
  )
import Grisette.Internal.Core.Data.Class.SymEq
  ( SymEq (symDistinct, (./=), (.==)),
  )
import Grisette.Internal.Core.Data.Class.SymOrd
  ( SymOrd (symCompare, (.<), (.<=), (.>), (.>=)),
  )
import Grisette.Internal.Core.Data.Class.SymRotate
  ( SymRotate (symRotate, symRotateNegated),
  )
import Grisette.Internal.Core.Data.Class.SymShift
  ( SymShift (symShift, symShiftNegated),
  )
import Grisette.Internal.Core.Data.Class.ToCon (ToCon (toCon))
import Grisette.Internal.Core.Data.Class.ToSym (ToSym (toSym))
import Grisette.Internal.Core.Data.Class.TryMerge (TryMerge, mrgSingle, tryMerge)
import Grisette.Internal.Core.Data.Symbol (Identifier, Symbol)
import Grisette.Internal.SymPrim.AllSyms (AllSyms (allSyms, allSymsS))
import Grisette.Internal.SymPrim.BV
  ( IntN,
    WordN,
  )
import Grisette.Internal.SymPrim.SymBV
  ( SymIntN,
    SymWordN,
  )
import Grisette.Internal.Utils.Parameterized
  ( KnownProof (KnownProof),
    LeqProof (LeqProof),
    NatRepr,
    SomePositiveNatRepr (SomePositiveNatRepr),
    knownAdd,
    leqAddPos,
    mkPositiveNatRepr,
    unsafeKnownProof,
    unsafeLeqProof,
  )
import Grisette.Lib.Data.Functor (mrgFmap)
import Language.Haskell.TH.Syntax (Lift (liftTyped))
import Test.QuickCheck (Arbitrary (arbitrary), Gen)
import Unsafe.Coerce (unsafeCoerce)

-- | An exception that would be thrown when operations are performed on
-- incompatible bit widths.
data SomeBVException = BitwidthMismatch | UndeterminedBitwidth T.Text
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, NFData)
  deriving
    ( Mergeable,
      ExtractSym,
      PPrint,
      SubstSym,
      EvalSym,
      SymEq,
      SymOrd,
      ToCon SomeBVException,
      ToSym SomeBVException
    )
    via (Default (SomeBVException))

instance Exception SomeBVException where
  displayException BitwidthMismatch = "Bit width does not match"
  displayException (UndeterminedBitwidth msg) =
    "Cannot determine bit-width for literals: " <> T.unpack msg

class MaySomeBV bv where
  assignLitBitWidth :: (KnownNat n, 1 <= n) => SomeBVLit -> bv n

instance MaySomeBV IntN where
  assignLitBitWidth = \case
    SomeBVIntLit i -> fromInteger i
    SomeBVCondLit _ -> error "Should not happen"

instance MaySomeBV WordN where
  assignLitBitWidth = \case
    SomeBVIntLit i -> fromInteger i
    SomeBVCondLit _ -> error "Should not happen"

instance MaySomeBV SymIntN where
  assignLitBitWidth = \case
    SomeBVIntLit i -> fromInteger i
    SomeBVCondLit u -> simpleMerge $ do
      i <- u
      mrgSingle $ fromInteger i

instance MaySomeBV SymWordN where
  assignLitBitWidth = \case
    SomeBVIntLit i -> fromInteger i
    SomeBVCondLit u -> simpleMerge $ do
      i <- u
      mrgSingle $ fromInteger i

assignBitWidthList ::
  forall bv.
  (forall n. (KnownNat n, 1 <= n) => Num (bv n), MaySomeBV bv) =>
  T.Text ->
  [SomeBV bv] ->
  Either SomeBVException [SomeBV bv]
assignBitWidthList msg bvs = case allNonMaybeBitWidth of
  [] -> Left $ UndeterminedBitwidth msg
  (x : xs) ->
    if all (== x) xs
      then case allHasBitWidth of
        (SomeBV (i :: bv i) : _) -> Right $ fmap (assignSingleBitWidth i) bvs
        _ -> error "Should not happen"
      else Left BitwidthMismatch
  where
    maybeBitWidth :: SomeBV bv -> Maybe Int
    maybeBitWidth (SomeBV (_ :: bv n)) = Just $ fromIntegral $ natVal (Proxy @n)
    maybeBitWidth (SomeBVLit _) = Nothing
    allMaybeBitWidth = map maybeBitWidth bvs
    allNonMaybeBitWidth = catMaybes allMaybeBitWidth
    allHasBitWidth = filter (isJust . maybeBitWidth) bvs
    assignSingleBitWidth ::
      forall i. (KnownNat i, 1 <= i) => bv i -> SomeBV bv -> SomeBV bv
    assignSingleBitWidth _ s@(SomeBV _) = s
    assignSingleBitWidth _ (SomeBVLit i) = SomeBV (assignLitBitWidth i :: bv i)

class AssignBitWidth a where
  assignBitWidth :: T.Text -> a -> Either SomeBVException a

instance
  (forall n. (KnownNat n, 1 <= n) => Num (bv n), MaySomeBV bv) =>
  AssignBitWidth (SomeBV bv, SomeBV bv)
  where
  assignBitWidth msg (a, b) = do
    l <- assignBitWidthList msg [a, b]
    case l of
      [a', b'] -> Right (a', b')
      _ -> error "Should not happen"

instance
  (forall n. (KnownNat n, 1 <= n) => Num (bv n), MaySomeBV bv) =>
  AssignBitWidth (SomeBV bv, SomeBV bv, SomeBV bv)
  where
  assignBitWidth msg (a, b, c) = do
    l <- assignBitWidthList msg [a, b, c]
    case l of
      [a', b', c'] -> Right (a', b', c')
      _ -> error "Should not happen"

instance
  (forall n. (KnownNat n, 1 <= n) => Num (bv n), MaySomeBV bv) =>
  AssignBitWidth (SomeBV bv, SomeBV bv, SomeBV bv, SomeBV bv)
  where
  assignBitWidth msg (a, b, c, d) = do
    l <- assignBitWidthList msg [a, b, c, d]
    case l of
      [a', b', c', d'] -> Right (a', b', c', d')
      _ -> error "Should not happen"

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.SymPrim

-- | Non-indexed bitvectors.
--
-- The creation of t'SomeBV' can be done with the `bv` function with a positive
-- bit width and a value:
--
-- >>> bv 4 0xf :: SomeBV IntN
-- 0xf
--
-- Operations on two t'SomeBV' values require the bitwidths to be the same. So
-- you should check for the bit width (via `finiteBitSize`) before performing
-- operations:
--
-- >>> bv 4 0x3 + bv 4 0x3 :: SomeBV IntN
-- 0x6
-- >>> bv 4 0x3 + bv 8 0x3 :: SomeBV IntN
-- *** Exception: BitwidthMismatch
--
-- One exception is that the equality testing (both concrete and symbolic via
-- 'SymEq') does not require the bitwidths to be the same. Different bitwidths
-- means the values are not equal:
--
-- >>> (bv 4 0x3 :: SomeBV IntN) == (bv 8 0x3)
-- False
--
-- __Note__: t'SomeBV' can be constructed out of integer literals without the
-- bit width provided. Further binary operations will usually require at least
-- one operand has the bit-width, and will use that as the bit-width for the
-- result.
--
-- For example:
--
-- 3 :: SomeBV IntN
-- bvlit(3)
-- >>> bv 4 0x1 + 3 :: SomeBV IntN
-- 0x4
-- >>> 3 * bv 4 0x1  :: SomeBV IntN
-- 0x3
-- >>> 3 * 3 :: SomeBV IntN
-- *** Exception: UndeterminedBitwidth "(*)"
--
-- Some operations allows the literals to be used without the bit-width, such as
-- '(+)', '(-)', 'negate', 'toUnsigned', 'toSigned', '.&.', '.|.', 'xor',
-- 'complement', 'setBit', 'clearBit', 'complementBit', 'shiftL', and
-- 'unsafeShiftL'.
--
-- >>> 3 + 3 :: SomeBV IntN
-- bvlit(6)
data SomeBV bv where
  SomeBV :: (KnownNat n, 1 <= n) => bv n -> SomeBV bv
  SomeBVLit :: SomeBVLit -> SomeBV bv

data SomeBVLit where
  SomeBVIntLit :: Integer -> SomeBVLit
  SomeBVCondLit :: Union Integer -> SomeBVLit
  deriving (Eq, Generic, Lift)
  deriving anyclass (Hashable, NFData)
  deriving (Mergeable, ExtractSym, AllSyms) via (Default SomeBVLit)

instance PPrint SomeBVLit where
  pformat (SomeBVIntLit i) = pformat i
  pformat (SomeBVCondLit u) = pformat u

toUnionInteger :: SomeBVLit -> Union Integer
toUnionInteger (SomeBVIntLit i) = mrgSingle i
toUnionInteger (SomeBVCondLit u) = u

instance Num SomeBVLit where
  SomeBVIntLit a + SomeBVIntLit b = SomeBVIntLit $ a + b
  l + r = SomeBVCondLit $ toUnionInteger l + toUnionInteger r
  SomeBVIntLit a - SomeBVIntLit b = SomeBVIntLit $ a - b
  l - r = SomeBVCondLit $ toUnionInteger l - toUnionInteger r
  SomeBVIntLit a * SomeBVIntLit b = SomeBVIntLit $ a * b
  l * r = SomeBVCondLit $ toUnionInteger l * toUnionInteger r
  negate (SomeBVIntLit a) = SomeBVIntLit $ negate a
  negate l = SomeBVCondLit $ negate $ toUnionInteger l
  abs (SomeBVIntLit a) = SomeBVIntLit $ abs a
  abs l = SomeBVCondLit $ abs $ toUnionInteger l
  signum (SomeBVIntLit a) = SomeBVIntLit $ signum a
  signum l = SomeBVCondLit $ signum $ toUnionInteger l
  fromInteger = SomeBVIntLit

instance Bits SomeBVLit where
  SomeBVIntLit l .&. SomeBVIntLit r = SomeBVIntLit $ l .&. r
  l .&. r = SomeBVCondLit $ do
    l <- toUnionInteger l
    r <- toUnionInteger r
    mrgSingle $ l .&. r
  SomeBVIntLit l .|. SomeBVIntLit r = SomeBVIntLit $ l .|. r
  l .|. r = SomeBVCondLit $ do
    l <- toUnionInteger l
    r <- toUnionInteger r
    mrgSingle $ l .|. r
  SomeBVIntLit l `xor` SomeBVIntLit r = SomeBVIntLit $ l `xor` r
  l `xor` r = SomeBVCondLit $ do
    l <- toUnionInteger l
    r <- toUnionInteger r
    mrgSingle $ l `xor` r
  complement (SomeBVIntLit l) = SomeBVIntLit $ complement l
  complement l = SomeBVCondLit $ do
    l <- toUnionInteger l
    mrgSingle $ complement l
  setBit (SomeBVIntLit l) i = SomeBVIntLit $ setBit l i
  setBit l i = SomeBVCondLit $ do
    l <- toUnionInteger l
    mrgSingle $ setBit l i
  clearBit (SomeBVIntLit l) i = SomeBVIntLit $ clearBit l i
  clearBit l i = SomeBVCondLit $ do
    l <- toUnionInteger l
    mrgSingle $ clearBit l i
  complementBit (SomeBVIntLit l) i = SomeBVIntLit $ complementBit l i
  complementBit l i = SomeBVCondLit $ do
    l <- toUnionInteger l
    mrgSingle $ complementBit l i
  shiftL (SomeBVIntLit a) i = SomeBVIntLit $ shiftL a i
  shiftL l i = SomeBVCondLit $ do
    l <- toUnionInteger l
    mrgSingle $ shiftL l i
  unsafeShiftL (SomeBVIntLit a) i = SomeBVIntLit $ unsafeShiftL a i
  unsafeShiftL l i = SomeBVCondLit $ do
    l <- toUnionInteger l
    mrgSingle $ unsafeShiftL l i
  shift = throw $ UndeterminedBitwidth "shift"
  rotate = throw $ UndeterminedBitwidth "rotate"
  bitSize = throw $ UndeterminedBitwidth "bitSize"
  bitSizeMaybe = throw $ UndeterminedBitwidth "bitSizeMaybe"
  isSigned = error "isSigned is not defined for SomeBVLit"
  testBit = throw $ UndeterminedBitwidth "testBit"
  bit = throw $ UndeterminedBitwidth "bit"
  popCount = throw $ UndeterminedBitwidth "popCount"

instance Show SomeBVLit where
  show (SomeBVIntLit i) = show i
  show (SomeBVCondLit u) = show u

instance Serial SomeBVLit where
  serialize (SomeBVIntLit i) = putWord8 0 >> serialize i
  serialize (SomeBVCondLit u) =
    putWord8 1 >> serialize u
  deserialize = do
    tag <- getWord8
    case tag of
      0 -> SomeBVIntLit <$> deserialize
      1 -> SomeBVCondLit <$> deserialize
      _ -> fail "Invalid tag"

instance Cereal.Serialize SomeBVLit where
  put = serialize
  get = deserialize

instance Binary.Binary SomeBVLit where
  put = serialize
  get = deserialize

instance
  (forall n. (KnownNat n, 1 <= n) => Serial (bv n)) =>
  Serial (SomeBV bv)
  where
  serialize (SomeBV (bv :: bv n)) =
    putWord8 0 >> serialize (natVal (Proxy @n)) >> serialize bv
  serialize (SomeBVLit i) = putWord8 1 >> serialize i
  deserialize = do
    tag <- getWord8
    case tag of
      0 -> do
        n :: Natural <- deserialize
        when (n == 0) $ fail "Invalid bit width"
        case mkPositiveNatRepr n of
          SomePositiveNatRepr (_ :: NatRepr x) -> do
            x <- deserialize @(bv x)
            return $ SomeBV x
      1 -> SomeBVLit <$> deserialize
      _ -> fail "Invalid tag"

instance
  (forall n. (KnownNat n, 1 <= n) => Serial (bv n)) =>
  Cereal.Serialize (SomeBV bv)
  where
  put = serialize
  get = deserialize

instance
  (forall n. (KnownNat n, 1 <= n) => Serial (bv n)) =>
  Binary.Binary (SomeBV bv)
  where
  put = serialize
  get = deserialize

instance
  ( forall n. (KnownNat n, 1 <= n) => Hashable (bv n),
    forall n. (KnownNat n, 1 <= n) => Num (bv n),
    MaySomeBV bv
  ) =>
  Hashable (SomeBV bv)
  where
  hashWithSalt s (SomeBV (bv :: bv n)) =
    s `hashWithSalt` (natVal (Proxy @n)) `hashWithSalt` bv
  hashWithSalt s (SomeBVLit i) = s `hashWithSalt` i
  {-# INLINE hashWithSalt #-}

instance
  (forall n. (KnownNat n, 1 <= n) => Lift (bv n)) =>
  Lift (SomeBV bv)
  where
  liftTyped (SomeBV bv) = [||SomeBV bv||]
  liftTyped (SomeBVLit i) = [||SomeBVLit i||]
  {-# INLINE liftTyped #-}

instance
  (forall n. (KnownNat n, 1 <= n) => Show (bv n)) =>
  Show (SomeBV bv)
  where
  show (SomeBV bv) = show bv
  show (SomeBVLit i) = "bvlit(" <> show i <> ")"
  {-# INLINE show #-}

-- , MaySomeBV bv

instance
  (forall n. (KnownNat n, 1 <= n) => NFData (bv n)) =>
  NFData (SomeBV bv)
  where
  rnf (SomeBV bv) = rnf bv
  rnf (SomeBVLit i) = rnf i
  {-# INLINE rnf #-}

instance
  ( forall n. (KnownNat n, 1 <= n) => Eq (bv n),
    forall n. (KnownNat n, 1 <= n) => Num (bv n),
    MaySomeBV bv
  ) =>
  Eq (SomeBV bv)
  where
  SomeBV (l :: bv l) == SomeBV (r :: bv r) =
    case sameNat (Proxy @l) (Proxy @r) of
      Just Refl -> l == r
      Nothing -> False
  SomeBV (l :: bv l) == SomeBVLit r = l == assignLitBitWidth r
  l == r@SomeBV {} = r == l
  _ == _ = throw $ UndeterminedBitwidth "=="
  {-# INLINE (==) #-}
  SomeBV (l :: bv l) /= SomeBV (r :: bv r) =
    case sameNat (Proxy @l) (Proxy @r) of
      Just Refl -> l /= r
      Nothing -> True
  SomeBV (l :: bv l) /= SomeBVLit r = l /= assignLitBitWidth r
  l /= r@SomeBV {} = r /= l
  _ /= _ = throw $ UndeterminedBitwidth "/="
  {-# INLINE (/=) #-}

instance
  ( forall n. (KnownNat n, 1 <= n) => Ord (bv n),
    forall n. (KnownNat n, 1 <= n) => Num (bv n),
    MaySomeBV bv
  ) =>
  Ord (SomeBV bv)
  where
  (<) = binSomeBV (<) (const $ const $ throw $ UndeterminedBitwidth "<")
  {-# INLINE (<) #-}
  (<=) = binSomeBV (<=) (const $ const $ throw $ UndeterminedBitwidth "(<=)")
  {-# INLINE (<=) #-}
  (>) = binSomeBV (>) (const $ const $ throw $ UndeterminedBitwidth ">")
  {-# INLINE (>) #-}
  (>=) = binSomeBV (>=) (const $ const $ throw $ UndeterminedBitwidth "(>=)")
  {-# INLINE (>=) #-}
  max = binSomeBVR1 max (const $ const $ throw $ UndeterminedBitwidth "max")
  {-# INLINE max #-}
  min = binSomeBVR1 min (const $ const $ throw $ UndeterminedBitwidth "min")
  {-# INLINE min #-}
  compare =
    binSomeBV compare (const $ const $ throw $ UndeterminedBitwidth "compare")
  {-# INLINE compare #-}

instance (forall n. (KnownNat n, 1 <= n) => Num (bv n), MaySomeBV bv) => Num (SomeBV bv) where
  (+) = binSomeBVR1 (+) (+)
  {-# INLINE (+) #-}
  (-) = binSomeBVR1 (-) (-)
  {-# INLINE (-) #-}
  (*) = binSomeBVR1 (*) (const $ const $ throw $ UndeterminedBitwidth "(*)")
  {-# INLINE (*) #-}
  negate = unarySomeBVR1 negate negate
  {-# INLINE negate #-}
  abs = unarySomeBVR1 abs (const $ throw $ UndeterminedBitwidth "abs")
  {-# INLINE abs #-}
  signum = unarySomeBVR1 signum (const $ throw $ UndeterminedBitwidth "signum")
  {-# INLINE signum #-}
  fromInteger = SomeBVLit . SomeBVIntLit
  {-# INLINE fromInteger #-}

instance
  ( forall n. (KnownNat n, 1 <= n) => Bits (bv n),
    forall n. (KnownNat n, 1 <= n) => Num (bv n),
    MaySomeBV bv
  ) =>
  Bits (SomeBV bv)
  where
  (.&.) = binSomeBVR1 (.&.) (.&.)
  (.|.) = binSomeBVR1 (.|.) (.|.)
  xor = binSomeBVR1 xor xor
  complement = unarySomeBVR1 complement complement
  shift s i =
    unarySomeBVR1 (`shift` i) (const $ throw $ UndeterminedBitwidth "shift") s
  rotate s i =
    unarySomeBVR1 (`rotate` i) (const $ throw $ UndeterminedBitwidth "rotate") s
  zeroBits =
    error $
      "zeroBits is not defined for SomeBV as no bitwidth is known, use "
        <> "(bv <bitwidth> 0) or (SomeBV (zeroBits :: bv <bitwidth>)) instead"
  bit =
    error $
      "bit is not defined for SomeBV as no bitwidth is known, use "
        <> "(SomeBV (bit <bit> :: bv <bitwidth>)) instead"
  setBit s i = unarySomeBVR1 (`setBit` i) (`setBit` i) s
  clearBit s i = unarySomeBVR1 (`clearBit` i) (`clearBit` i) s
  complementBit s i = unarySomeBVR1 (`complementBit` i) (`complementBit` i) s
  testBit s i =
    unarySomeBV (`testBit` i) (const $ throw $ UndeterminedBitwidth "testBit") s
  bitSizeMaybe =
    unarySomeBV
      bitSizeMaybe
      (const $ throw $ UndeterminedBitwidth "bitSizeMaybe")
  bitSize =
    fromJust
      . unarySomeBV
        bitSizeMaybe
        (const $ throw $ UndeterminedBitwidth "bitSize")
  isSigned _ = isSigned (undefined :: bv 1)
  shiftL s i = unarySomeBVR1 (`shiftL` i) (`shiftL` i) s
  unsafeShiftL s i = unarySomeBVR1 (`unsafeShiftL` i) (`unsafeShiftL` i) s
  shiftR s i =
    unarySomeBVR1 (`shiftR` i) (const $ throw $ UndeterminedBitwidth "shiftR") s
  unsafeShiftR s i =
    unarySomeBVR1
      (`unsafeShiftR` i)
      (const $ throw $ UndeterminedBitwidth "unsafeShiftR")
      s
  rotateL s i =
    unarySomeBVR1
      (`rotateL` i)
      (const $ throw $ UndeterminedBitwidth "rotateL")
      s
  rotateR s i =
    unarySomeBVR1
      (`rotateR` i)
      (const $ throw $ UndeterminedBitwidth "rotateR")
      s
  popCount =
    unarySomeBV popCount (const $ throw $ UndeterminedBitwidth "popCount")

instance
  ( forall n. (KnownNat n, 1 <= n) => FiniteBits (bv n),
    forall n. (KnownNat n, 1 <= n) => Num (bv n),
    MaySomeBV bv
  ) =>
  FiniteBits (SomeBV bv)
  where
  finiteBitSize =
    unarySomeBV
      finiteBitSize
      (const $ throw $ UndeterminedBitwidth "finiteBitSize")
  {-# INLINE finiteBitSize #-}
  countLeadingZeros =
    unarySomeBV
      countLeadingZeros
      (const $ throw $ UndeterminedBitwidth "countLeadingZeros")
  {-# INLINE countLeadingZeros #-}
  countTrailingZeros =
    unarySomeBV
      countTrailingZeros
      (const $ throw $ UndeterminedBitwidth "countTrailingZeros")
  {-# INLINE countTrailingZeros #-}

instance
  (forall n. (KnownNat n, 1 <= n) => Enum (bv n)) =>
  Enum (SomeBV bv)
  where
  toEnum =
    error $
      "toEnum is not defined for SomeBV, use "
        <> "(SomeBV (toEnum <value> :: bv <bitwidth>)) instead"
  {-# INLINE toEnum #-}
  fromEnum =
    unarySomeBV fromEnum (const $ throw $ UndeterminedBitwidth "fromEnum")
  {-# INLINE fromEnum #-}

instance
  (forall n. (KnownNat n, 1 <= n) => Real (bv n), MaySomeBV bv) =>
  Real (SomeBV bv)
  where
  toRational =
    unarySomeBV toRational (const $ throw $ UndeterminedBitwidth "toRational")
  {-# INLINE toRational #-}

instance
  (forall n. (KnownNat n, 1 <= n) => Integral (bv n), MaySomeBV bv) =>
  Integral (SomeBV bv)
  where
  toInteger =
    unarySomeBV
      toInteger
      (const $ throw $ UndeterminedBitwidth "toInteger")
  {-# INLINE toInteger #-}
  quot = binSomeBVR1 quot (const $ throw $ UndeterminedBitwidth "quot")
  {-# INLINE quot #-}
  rem = binSomeBVR1 rem (const $ throw $ UndeterminedBitwidth "rem")
  {-# INLINE rem #-}
  div = binSomeBVR1 div (const $ throw $ UndeterminedBitwidth "div")
  {-# INLINE div #-}
  mod = binSomeBVR1 mod (const $ throw $ UndeterminedBitwidth "mod")
  {-# INLINE mod #-}
  quotRem = binSomeBVR2 quotRem (const $ throw $ UndeterminedBitwidth "quotRem")
  {-# INLINE quotRem #-}
  divMod = binSomeBVR2 divMod (const $ throw $ UndeterminedBitwidth "divMod")
  {-# INLINE divMod #-}

instance (SizedBV bv) => BV (SomeBV bv) where
  bvConcat (SomeBV (a :: bv l)) (SomeBV (b :: bv r)) =
    case ( leqAddPos (Proxy @l) (Proxy @r),
           knownAdd @l @r KnownProof KnownProof
         ) of
      (LeqProof, KnownProof) ->
        SomeBV $ sizedBVConcat a b
  bvConcat _ _ = throw $ UndeterminedBitwidth "bvConcat"
  {-# INLINE bvConcat #-}
  bvZext l (SomeBV (a :: bv n))
    | l < n = error "bvZext: trying to zero extend a value to a smaller size"
    | otherwise = res (Proxy @n)
    where
      n = fromIntegral $ natVal (Proxy @n)
      res :: forall (l :: Nat). Proxy l -> SomeBV bv
      res p =
        case ( unsafeKnownProof @l (fromIntegral l),
               unsafeLeqProof @1 @l,
               unsafeLeqProof @n @l
             ) of
          (KnownProof, LeqProof, LeqProof) -> SomeBV $ sizedBVZext p a
  bvZext _ _ = throw $ UndeterminedBitwidth "bvZext"
  {-# INLINE bvZext #-}
  bvSext l (SomeBV (a :: bv n))
    | l < n = error "bvSext: trying to zero extend a value to a smaller size"
    | otherwise = res (Proxy @n)
    where
      n = fromIntegral $ natVal (Proxy @n)
      res :: forall (l :: Nat). Proxy l -> SomeBV bv
      res p =
        case ( unsafeKnownProof @l (fromIntegral l),
               unsafeLeqProof @1 @l,
               unsafeLeqProof @n @l
             ) of
          (KnownProof, LeqProof, LeqProof) -> SomeBV $ sizedBVSext p a
  bvSext _ _ = throw $ UndeterminedBitwidth "bvSext"
  {-# INLINE bvSext #-}
  bvExt l (SomeBV (a :: bv n))
    | l < n = error "bvExt: trying to zero extend a value to a smaller size"
    | otherwise = res (Proxy @n)
    where
      n = fromIntegral $ natVal (Proxy @n)
      res :: forall (l :: Nat). Proxy l -> SomeBV bv
      res p =
        case ( unsafeKnownProof @l (fromIntegral l),
               unsafeLeqProof @1 @l,
               unsafeLeqProof @n @l
             ) of
          (KnownProof, LeqProof, LeqProof) -> SomeBV $ sizedBVExt p a
  bvExt _ _ = throw $ UndeterminedBitwidth "bvExt"
  {-# INLINE bvExt #-}
  bvSelect ix w (SomeBV (a :: bv n))
    | ix + w > n =
        error $
          "bvSelect: trying to select a bitvector outside the bounds, "
            <> "ix = "
            <> show ix
            <> ", w = "
            <> show w
            <> ", n = "
            <> show n
    | w == 0 = error "bvSelect: trying to select a bitvector of size 0"
    | otherwise = res (Proxy @n) (Proxy @n)
    where
      n = fromIntegral $ natVal (Proxy @n)
      res :: forall (w :: Nat) (ix :: Nat). Proxy w -> Proxy ix -> SomeBV bv
      res _ _ =
        case ( unsafeKnownProof @ix (fromIntegral ix),
               unsafeKnownProof @w (fromIntegral w),
               unsafeLeqProof @1 @w,
               unsafeLeqProof @(ix + w) @n
             ) of
          (KnownProof, KnownProof, LeqProof, LeqProof) ->
            SomeBV $ sizedBVSelect (Proxy @ix) (Proxy @w) a
  bvSelect _ _ _ = throw $ UndeterminedBitwidth "bvSelect"
  bv n i = unsafeSomeBV n $ \_ -> sizedBVFromIntegral i
  {-# INLINE bv #-}

instance
  (forall n. (KnownNat n, 1 <= n) => EvalSym (bv n)) =>
  EvalSym (SomeBV bv)
  where
  evalSym fillDefault model = unarySomeBVR1 (evalSym fillDefault model) id
  {-# INLINE evalSym #-}

instance
  (forall n. (KnownNat n, 1 <= n) => ExtractSym (bv n)) =>
  ExtractSym (SomeBV bv)
  where
  extractSymMaybe = unarySomeBV extractSymMaybe extractSymMaybe
  {-# INLINE extractSymMaybe #-}

instance
  (forall n. (KnownNat n, 1 <= n) => PPrint (bv n)) =>
  PPrint (SomeBV bv)
  where
  pformat (SomeBV bv) = pformat bv
  pformat (SomeBVLit i) = "bvlit(" <> pformat i <> ")"
  {-# INLINE pformat #-}

data CompileTimeNat where
  CompileTimeNat :: (KnownNat n, 1 <= n) => Proxy n -> CompileTimeNat

instance Show CompileTimeNat where
  show (CompileTimeNat (Proxy :: Proxy n)) = show (natVal (Proxy @n))
  {-# INLINE show #-}

instance Eq CompileTimeNat where
  CompileTimeNat (Proxy :: Proxy n) == CompileTimeNat (Proxy :: Proxy m) =
    case sameNat (Proxy @n) (Proxy @m) of
      Just Refl -> True
      Nothing -> False
  {-# INLINE (==) #-}

instance Ord CompileTimeNat where
  compare
    (CompileTimeNat (Proxy :: Proxy n))
    (CompileTimeNat (Proxy :: Proxy m)) =
      compare (natVal (Proxy @n)) (natVal (Proxy @m))
  {-# INLINE compare #-}

instance
  (forall n. (KnownNat n, 1 <= n) => Mergeable (bv n)) =>
  Mergeable (SomeBV bv)
  where
  rootStrategy =
    SortedStrategy @(Maybe CompileTimeNat)
      ( \case
          (SomeBVLit _) -> Nothing
          (SomeBV (_ :: bv n)) -> Just (CompileTimeNat (Proxy @n))
      )
      ( \case
          Nothing -> SimpleStrategy $
            \c (SomeBVLit l) (SomeBVLit r) ->
              SomeBVLit $
                SomeBVCondLit $
                  mrgIf c (toUnionInteger l) (toUnionInteger r)
          Just (CompileTimeNat (_ :: proxy n)) ->
            wrapStrategy
              (rootStrategy @(bv n))
              SomeBV
              (\(SomeBV x) -> unsafeCoerce x)
      )

-- | The 'symDistinct' instance for t'SomeBV' will have the following behavior:
--
-- * If the list is empty or has only one element, it will return 'True'.
-- * If none of the elements have a bit-width, it will throw
--   'UndeterminedBitwidth' exception.
-- * If the elements have different bit-widths, it will throw a
--   'BitwidthMismatch' exception.
-- * If there are at least one element have a bit-width, and all elements with
--   known bit-width have the same bit-width, it will generate a single symbolic
--   formula using @distinct@.
instance
  ( forall n. (KnownNat n, 1 <= n) => SymEq (bv n),
    forall n. (KnownNat n, 1 <= n) => Num (bv n),
    MaySomeBV bv
  ) =>
  SymEq (SomeBV bv)
  where
  SomeBV (l :: bv l) .== SomeBV (r :: bv r) =
    case sameNat (Proxy @l) (Proxy @r) of
      Just Refl -> l .== r
      Nothing -> con False
  SomeBV (l :: bv l) .== SomeBVLit r = l .== assignLitBitWidth r
  SomeBVLit l .== SomeBV (r :: bv r) = assignLitBitWidth l .== r
  SomeBVLit _ .== SomeBVLit _ = throw $ UndeterminedBitwidth ".=="
  {-# INLINE (.==) #-}
  SomeBV (l :: bv l) ./= SomeBV (r :: bv r) =
    case sameNat (Proxy @l) (Proxy @r) of
      Just Refl -> l ./= r
      Nothing -> con True
  SomeBV (l :: bv l) ./= SomeBVLit r = l ./= assignLitBitWidth r
  SomeBVLit l ./= SomeBV (r :: bv r) = assignLitBitWidth l ./= r
  SomeBVLit _ ./= SomeBVLit _ = throw $ UndeterminedBitwidth "./="
  symDistinct l = case l of
    [] -> con True
    [_] -> con True
    _ -> case assignBitWidthList "symDistinct" l of
      Right (SomeBV (a :: bv a) : l) -> symDistinct $ a : go l
        where
          go :: [SomeBV bv] -> [bv a]
          go [] = []
          go (SomeBV (x :: bv x) : xs) = case sameNat (Proxy @x) (Proxy @a) of
            Just Refl -> x : go xs
            Nothing -> error "Should not happen"
          go (SomeBVLit _ : _) = error "Should not happen"
      Right _ -> error "Should not happen"
      Left UndeterminedBitwidth {} -> throw $ UndeterminedBitwidth "symDistinct"
      Left BitwidthMismatch -> throw BitwidthMismatch
  {-# INLINE (./=) #-}

instance
  ( forall n. (KnownNat n, 1 <= n) => SymOrd (bv n),
    forall n. (KnownNat n, 1 <= n) => Num (bv n),
    MaySomeBV bv
  ) =>
  SymOrd (SomeBV bv)
  where
  (.<) = binSomeBV (.<) (const $ const $ throw $ UndeterminedBitwidth "(.<)")
  {-# INLINE (.<) #-}
  (.<=) = binSomeBV (.<=) (const $ const $ throw $ UndeterminedBitwidth "(.<=)")
  {-# INLINE (.<=) #-}
  (.>) = binSomeBV (.>) (const $ const $ throw $ UndeterminedBitwidth "(.>)")
  {-# INLINE (.>) #-}
  (.>=) = binSomeBV (.>=) (const $ const $ throw $ UndeterminedBitwidth "(.>=)")
  {-# INLINE (.>=) #-}
  symCompare =
    binSomeBV
      symCompare
      (const $ const $ throw $ UndeterminedBitwidth "symCompare")
  {-# INLINE symCompare #-}

instance
  (forall n. (KnownNat n, 1 <= n) => SubstSym (bv n)) =>
  SubstSym (SomeBV bv)
  where
  substSym c s = unarySomeBVR1 (substSym c s) id
  {-# INLINE substSym #-}

instance
  ( KnownNat n,
    1 <= n,
    forall m. (KnownNat m, 1 <= m) => GenSym () (bv m),
    Mergeable (SomeBV bv)
  ) =>
  GenSym (Proxy n) (SomeBV bv)
  where
  fresh _ =
    (\(i :: Union (bv n)) -> mrgFmap SomeBV i) <$> fresh ()
  {-# INLINE fresh #-}

instance
  ( KnownNat n,
    1 <= n,
    forall m. (KnownNat m, 1 <= m) => GenSymSimple () (bv m),
    Mergeable (SomeBV bv)
  ) =>
  GenSymSimple (Proxy n) (SomeBV bv)
  where
  simpleFresh _ = (\(i :: bv n) -> SomeBV i) <$> simpleFresh ()
  {-# INLINE simpleFresh #-}

instance
  ( forall m. (KnownNat m, 1 <= m) => GenSym () (bv m),
    Mergeable (SomeBV bv)
  ) =>
  GenSym (SomeBV bv) (SomeBV bv)
  where
  fresh (SomeBV (_ :: bv x)) = fresh (Proxy @x)
  fresh (SomeBVLit _) = throw $ UndeterminedBitwidth "fresh"
  {-# INLINE fresh #-}

instance
  ( forall m. (KnownNat m, 1 <= m) => GenSymSimple () (bv m),
    Mergeable (SomeBV bv)
  ) =>
  GenSymSimple (SomeBV bv) (SomeBV bv)
  where
  simpleFresh (SomeBV (_ :: bv x)) = simpleFresh (Proxy @x)
  simpleFresh (SomeBVLit _) = throw $ UndeterminedBitwidth "simpleFresh"
  {-# INLINE simpleFresh #-}

instance
  ( forall n. (KnownNat n, 1 <= n) => GenSym () (bv n),
    Mergeable (SomeBV bv)
  ) =>
  GenSym Int (SomeBV bv)
  where
  fresh n
    | n <= 0 = error "fresh: cannot generate a bitvector of non-positive size"
    | otherwise = case mkPositiveNatRepr (fromIntegral n) of
        SomePositiveNatRepr (_ :: NatRepr x) -> fresh (Proxy @x)
  {-# INLINE fresh #-}

instance
  ( forall n. (KnownNat n, 1 <= n) => GenSymSimple () (bv n),
    Mergeable (SomeBV bv)
  ) =>
  GenSymSimple Int (SomeBV bv)
  where
  simpleFresh n
    | n <= 0 = error "fresh: cannot generate a bitvector of non-positive size"
    | otherwise = case mkPositiveNatRepr (fromIntegral n) of
        SomePositiveNatRepr (_ :: NatRepr x) -> simpleFresh (Proxy @x)
  {-# INLINE simpleFresh #-}

instance
  ( forall n. (KnownNat n, 1 <= n) => SignConversion (ubv n) (sbv n),
    -- Add this to help the type checker resolve the functional dependency
    SignConversion (ubv 1) (sbv 1)
  ) =>
  SignConversion (SomeBV ubv) (SomeBV sbv)
  where
  toSigned (SomeBV (n :: ubv n)) = SomeBV (toSigned n :: sbv n)
  toSigned (SomeBVLit i) = SomeBVLit i
  {-# INLINE toSigned #-}
  toUnsigned (SomeBV (n :: sbv n)) = SomeBV (toUnsigned n :: ubv n)
  toUnsigned (SomeBVLit i) = SomeBVLit i
  {-# INLINE toUnsigned #-}

instance
  (forall n. (KnownNat n, 1 <= n) => ToCon (sbv n) (cbv n)) =>
  ToCon (SomeBV sbv) (SomeBV cbv)
  where
  toCon (SomeBV (n :: sbv n)) = SomeBV <$> (toCon n :: Maybe (cbv n))
  toCon (SomeBVLit i) = Just $ SomeBVLit i
  {-# INLINE toCon #-}

instance
  (forall n. (KnownNat n, 1 <= n) => ToSym (cbv n) (sbv n)) =>
  ToSym (SomeBV cbv) (SomeBV sbv)
  where
  toSym (SomeBV (n :: cbv n)) = SomeBV (toSym n :: sbv n)
  toSym (SomeBVLit i) = SomeBVLit i
  {-# INLINE toSym #-}

divRemOrBase0 ::
  ( forall n.
    (KnownNat n, 1 <= n) =>
    (bv n, bv n) ->
    bv n ->
    bv n ->
    (bv n, bv n)
  ) ->
  (SomeBV bv, SomeBV bv) ->
  SomeBV bv ->
  SomeBV bv ->
  (SomeBV bv, SomeBV bv)
divRemOrBase0
  f
  (SomeBV (dd :: bv dd), SomeBV (dm :: bv dm))
  (SomeBV (a :: bv a))
  (SomeBV (b :: bv b)) =
    case ( sameNat (Proxy @a) (Proxy @b),
           sameNat (Proxy @a) (Proxy @dd),
           sameNat (Proxy @a) (Proxy @dm)
         ) of
      (Just Refl, Just Refl, Just Refl) -> bimap SomeBV SomeBV $ f (dd, dm) a b
      _ -> error "Should not happen"
divRemOrBase0 _ _ _ _ = error "Should not happen"
{-# INLINE divRemOrBase0 #-}

divRemOrBase ::
  (forall n. (KnownNat n, 1 <= n) => Num (bv n), MaySomeBV bv) =>
  ( forall n.
    (KnownNat n, 1 <= n) =>
    (bv n, bv n) ->
    bv n ->
    bv n ->
    (bv n, bv n)
  ) ->
  (SomeBV bv, SomeBV bv) ->
  SomeBV bv ->
  SomeBV bv ->
  (SomeBV bv, SomeBV bv)
divRemOrBase f (a, b) c d =
  case assignBitWidth "divRemOrBase" (a, b, c, d) of
    Right (a', b', c', d') -> divRemOrBase0 f (a', b') c' d'
    Left e -> throw e

instance
  ( forall n. (KnownNat n, 1 <= n) => DivOr (bv n),
    forall n. (KnownNat n, 1 <= n) => Num (bv n),
    MaySomeBV bv
  ) =>
  DivOr (SomeBV bv)
  where
  divOr = ternSomeBVR1 divOr
  {-# INLINE divOr #-}
  modOr = ternSomeBVR1 modOr
  {-# INLINE modOr #-}
  quotOr = ternSomeBVR1 quotOr
  {-# INLINE quotOr #-}
  remOr = ternSomeBVR1 remOr
  {-# INLINE remOr #-}
  divModOr = divRemOrBase divModOr
  {-# INLINE divModOr #-}
  quotRemOr = divRemOrBase quotRemOr
  {-# INLINE quotRemOr #-}

instance
  ( forall n.
    (KnownNat n, 1 <= n) =>
    SafeDiv e (bv n) (ExceptT e m),
    MonadError (Either SomeBVException e) m,
    TryMerge m,
    Mergeable e,
    forall n. (KnownNat n, 1 <= n) => Num (bv n),
    MaySomeBV bv
  ) =>
  SafeDiv (Either SomeBVException e) (SomeBV bv) m
  where
  safeDiv =
    binSomeBVSafeR1
      (safeDiv @e)
      (const $ const $ throwError $ Left $ UndeterminedBitwidth "safeDiv")
  {-# INLINE safeDiv #-}
  safeMod =
    binSomeBVSafeR1
      (safeMod @e)
      (const $ const $ throwError $ Left $ UndeterminedBitwidth "safeMod")
  {-# INLINE safeMod #-}
  safeQuot =
    binSomeBVSafeR1
      (safeQuot @e)
      (const $ const $ throwError $ Left $ UndeterminedBitwidth "safeQuot")
  {-# INLINE safeQuot #-}
  safeRem =
    binSomeBVSafeR1
      (safeRem @e)
      (const $ const $ throwError $ Left $ UndeterminedBitwidth "safeRem")
  {-# INLINE safeRem #-}
  safeDivMod =
    binSomeBVSafeR2
      (safeDivMod @e)
      (const $ const $ throwError $ Left $ UndeterminedBitwidth "safeDivMod")
  {-# INLINE safeDivMod #-}
  safeQuotRem =
    binSomeBVSafeR2
      (safeQuotRem @e)
      (const $ const $ throwError $ Left $ UndeterminedBitwidth "safeQuotRem")
  {-# INLINE safeQuotRem #-}

instance
  ( forall n.
    (KnownNat n, 1 <= n) =>
    SafeLinearArith e (bv n) (ExceptT e m),
    MonadError (Either SomeBVException e) m,
    TryMerge m,
    Mergeable e,
    forall n. (KnownNat n, 1 <= n) => Num (bv n),
    MaySomeBV bv
  ) =>
  SafeLinearArith (Either SomeBVException e) (SomeBV bv) m
  where
  safeAdd =
    binSomeBVSafeR1
      (safeAdd @e)
      (const $ const $ throwError $ Left $ UndeterminedBitwidth "safeAdd")
  {-# INLINE safeAdd #-}
  safeSub =
    binSomeBVSafeR1
      (safeSub @e)
      (const $ const $ throwError $ Left $ UndeterminedBitwidth "safeSub")
  {-# INLINE safeSub #-}
  safeNeg =
    unarySomeBV
      ( \v ->
          mrgFmap SomeBV $
            runExceptT (safeNeg @e v) >>= either (throwError . Right) pure
      )
      (const $ throwError $ Left $ UndeterminedBitwidth "safeNeg")
  {-# INLINE safeNeg #-}

instance
  ( forall n. (KnownNat n, 1 <= n) => SymShift (bv n),
    forall n. (KnownNat n, 1 <= n) => Num (bv n),
    MaySomeBV bv
  ) =>
  SymShift (SomeBV bv)
  where
  symShift =
    binSomeBVR1
      symShift
      (const $ const $ throw $ UndeterminedBitwidth "safeShift")
  {-# INLINE symShift #-}
  symShiftNegated =
    binSomeBVR1
      symShiftNegated
      (const $ const $ throw $ UndeterminedBitwidth "safeShiftNegated")
  {-# INLINE symShiftNegated #-}

instance
  ( forall n. (KnownNat n, 1 <= n) => SymRotate (bv n),
    forall n. (KnownNat n, 1 <= n) => Num (bv n),
    MaySomeBV bv
  ) =>
  SymRotate (SomeBV bv)
  where
  symRotate =
    binSomeBVR1
      symRotate
      (const $ const $ throw $ UndeterminedBitwidth "safeRotate")
  {-# INLINE symRotate #-}
  symRotateNegated =
    binSomeBVR1
      symRotateNegated
      (const $ const $ throw $ UndeterminedBitwidth "safeRotateNegated")
  {-# INLINE symRotateNegated #-}

instance
  ( forall n.
    (KnownNat n, 1 <= n) =>
    SafeSymShift e (bv n) (ExceptT e m),
    MonadError (Either SomeBVException e) m,
    TryMerge m,
    Mergeable e,
    forall n. (KnownNat n, 1 <= n) => Num (bv n),
    MaySomeBV bv
  ) =>
  SafeSymShift (Either SomeBVException e) (SomeBV bv) m
  where
  safeSymShiftL =
    binSomeBVSafeR1
      (safeSymShiftL @e)
      (const $ const $ throwError $ Left $ UndeterminedBitwidth "safeSymShiftL")
  {-# INLINE safeSymShiftL #-}
  safeSymShiftR =
    binSomeBVSafeR1
      (safeSymShiftR @e)
      (const $ const $ throwError $ Left $ UndeterminedBitwidth "safeSymShiftR")
  {-# INLINE safeSymShiftR #-}
  safeSymStrictShiftL =
    binSomeBVSafeR1
      (safeSymStrictShiftL @e)
      (const $ const $ throwError $ Left $ UndeterminedBitwidth "safeSymStrictShiftL")
  {-# INLINE safeSymStrictShiftL #-}
  safeSymStrictShiftR =
    binSomeBVSafeR1
      (safeSymStrictShiftR @e)
      (const $ const $ throwError $ Left $ UndeterminedBitwidth "safeSymStrictShiftR")
  {-# INLINE safeSymStrictShiftR #-}

instance
  ( forall n.
    (KnownNat n, 1 <= n) =>
    SafeSymRotate e (bv n) (ExceptT e m),
    MonadError (Either SomeBVException e) m,
    TryMerge m,
    Mergeable e,
    forall n. (KnownNat n, 1 <= n) => Num (bv n),
    MaySomeBV bv
  ) =>
  SafeSymRotate (Either SomeBVException e) (SomeBV bv) m
  where
  safeSymRotateL =
    binSomeBVSafeR1
      (safeSymRotateL @e)
      (const $ const $ throwError $ Left $ UndeterminedBitwidth "safeSymRotateL")
  {-# INLINE safeSymRotateL #-}
  safeSymRotateR =
    binSomeBVSafeR1
      (safeSymRotateR @e)
      (const $ const $ throwError $ Left $ UndeterminedBitwidth "safeSymRotateR")
  {-# INLINE safeSymRotateR #-}

instance
  ( forall n. (KnownNat n, 1 <= n) => ITEOp (bv n),
    forall n. (KnownNat n, 1 <= n) => Num (bv n),
    MaySomeBV bv
  ) =>
  ITEOp (SomeBV bv)
  where
  symIte cond =
    binSomeBVR1
      (symIte cond)
      (\l r -> SomeBVCondLit $ mrgIf cond (toUnionInteger l) (toUnionInteger r))

instance
  ( forall n. (KnownNat n, 1 <= n) => AllSyms (bv n),
    MaySomeBV bv
  ) =>
  AllSyms (SomeBV bv)
  where
  allSyms = unarySomeBV allSyms allSyms
  {-# INLINE allSyms #-}
  allSymsS = unarySomeBV allSymsS allSymsS
  {-# INLINE allSymsS #-}

-- Synonyms

-- | Type synonym for t'SomeBV' for concrete signed bitvectors.
type SomeIntN = SomeBV IntN

-- | Pattern synonym for t'SomeBV' for concrete signed bitvectors.
pattern SomeIntN :: () => (KnownNat n, 1 <= n) => IntN n -> SomeIntN
pattern SomeIntN a = SomeBV a

-- | Type synonym for t'SomeBV' for concrete unsigned bitvectors.
type SomeWordN = SomeBV WordN

-- | Pattern synonym for t'SomeBV' for concrete unsigned bitvectors.
pattern SomeWordN :: () => (KnownNat n, 1 <= n) => WordN n -> SomeWordN
pattern SomeWordN a = SomeBV a

-- | Type synonym for t'SomeBV' for symbolic signed bitvectors.
type SomeSymIntN = SomeBV SymIntN

-- | Pattern synonym for t'SomeBV' for symbolic signed bitvectors.
pattern SomeSymIntN :: () => (KnownNat n, 1 <= n) => SymIntN n -> SomeSymIntN
pattern SomeSymIntN a = SomeBV a

-- | Type synonym for t'SomeBV' for symbolic unsigned bitvectors.
type SomeSymWordN = SomeBV SymWordN

-- | Pattern synonym for t'SomeBV' for symbolic unsigned bitvectors.
pattern SomeSymWordN :: () => (KnownNat n, 1 <= n) => SymWordN n -> SomeSymWordN
pattern SomeSymWordN a = SomeBV a

-- Construction

-- | Construct a t'SomeBV' with a given run-time bitwidth and a polymorphic
-- value for the underlying bitvector.
unsafeSomeBV ::
  forall bv.
  Int ->
  (forall proxy n. (KnownNat n, 1 <= n) => proxy n -> bv n) ->
  SomeBV bv
unsafeSomeBV n i
  | n <= 0 = error "unsafeBV: trying to create a bitvector of non-positive size"
  | otherwise = case mkPositiveNatRepr (fromIntegral n) of
      SomePositiveNatRepr (_ :: NatRepr x) -> SomeBV (i (Proxy @x))
{-# INLINE unsafeSomeBV #-}

-- | Construct a symbolic t'SomeBV' with a given concrete t'SomeBV'. Similar to
-- 'con' but for t'SomeBV'.
--
-- >>> a = bv 8 0x12 :: SomeIntN
-- >>> conBV a :: SomeSymIntN
-- 0x12
conBV ::
  forall cbv bv.
  ( forall n. (KnownNat n, 1 <= n) => Solvable (cbv n) (bv n),
    Solvable (cbv 1) (bv 1)
  ) =>
  SomeBV cbv ->
  SomeBV bv
conBV (SomeBV (v :: cbv n)) = SomeBV $ con @(cbv n) @(bv n) v
conBV (SomeBVLit i) = SomeBVLit i

-- | View pattern for symbolic t'SomeBV' to see if it contains a concrete value
-- and extract it. Similar to 'conView' but for t'SomeBV'.
--
-- >>> conBVView (bv 8 0x12 :: SomeSymIntN)
-- Just 0x12
-- >>> conBVView (ssymBV 4 "a" :: SomeSymIntN)
-- Nothing
conBVView ::
  forall cbv bv.
  ( forall n. (KnownNat n, 1 <= n) => Solvable (cbv n) (bv n),
    Solvable (cbv 1) (bv 1)
  ) =>
  SomeBV bv ->
  Maybe (SomeBV cbv)
conBVView (SomeBV (bv :: bv n)) = case conView @(cbv n) bv of
  Just c -> Just $ SomeBV c
  Nothing -> Nothing
conBVView (SomeBVLit i) = Just $ SomeBVLit i

-- | Pattern synonym for symbolic t'SomeBV' to see if it contains a concrete
-- value and extract it. Similar to 'Grisette.Core.Con' but for t'SomeBV'.
--
-- >>> case (bv 8 0x12 :: SomeSymIntN) of { ConBV c -> c; _ -> error "impossible" }
-- 0x12
pattern ConBV ::
  forall cbv bv.
  ( forall n. (KnownNat n, 1 <= n) => Solvable (cbv n) (bv n),
    Solvable (cbv 1) (bv 1)
  ) =>
  SomeBV cbv ->
  SomeBV bv
pattern ConBV c <- (conBVView -> Just c)
  where
    ConBV c = conBV c

-- | Construct a symbolic t'SomeBV' with a given run-time bitwidth and a symbol.
-- Similar to 'sym' but for t'SomeBV'.
--
-- >>> symBV 8 "a" :: SomeSymIntN
-- a
symBV ::
  forall cbv bv.
  ( forall n. (KnownNat n, 1 <= n) => Solvable (cbv n) (bv n),
    Solvable (cbv 1) (bv 1)
  ) =>
  Int ->
  Symbol ->
  SomeBV bv
symBV n s = unsafeSomeBV n $ \(_ :: proxy n) -> sym @(cbv n) s

-- | Construct a symbolic t'SomeBV' with a given run-time bitwidth and an
-- identifier. Similar to 'ssym' but for t'SomeBV'.
--
-- >>> ssymBV 8 "a" :: SomeSymIntN
-- a
ssymBV ::
  forall cbv bv.
  ( forall n. (KnownNat n, 1 <= n) => Solvable (cbv n) (bv n),
    Solvable (cbv 1) (bv 1)
  ) =>
  Int ->
  Identifier ->
  SomeBV bv
ssymBV n s = unsafeSomeBV n $ \(_ :: proxy n) -> ssym @(cbv n) s

-- | Construct a symbolic t'SomeBV' with a given run-time bitwidth, an identifier
-- and an index. Similar to 'isym' but for t'SomeBV'.
--
-- >>> isymBV 8 "a" 1 :: SomeSymIntN
-- a@1
isymBV ::
  forall cbv bv.
  ( forall n. (KnownNat n, 1 <= n) => Solvable (cbv n) (bv n),
    Solvable (cbv 1) (bv 1)
  ) =>
  Int ->
  Identifier ->
  Int ->
  SomeBV bv
isymBV n s i = unsafeSomeBV n $ \(_ :: proxy n) -> isym @(cbv n) s i

-- | Generate an arbitrary t'SomeBV' with a given run-time bitwidth.
arbitraryBV ::
  forall bv.
  (forall n. (KnownNat n, 1 <= n) => Arbitrary (bv n)) =>
  Int ->
  Gen (SomeBV bv)
arbitraryBV n
  | n <= 0 =
      error "arbitraryBV: trying to create a bitvector of non-positive size"
  | otherwise = case mkPositiveNatRepr (fromIntegral n) of
      SomePositiveNatRepr (_ :: NatRepr x) -> do
        v <- arbitrary :: Gen (bv x)
        return $ SomeBV v

-- Helpers

-- | Lift a unary operation on sized bitvectors that returns anything to
-- t'SomeBV'.
unarySomeBV ::
  forall bv r.
  (forall n. (KnownNat n, 1 <= n) => bv n -> r) ->
  (SomeBVLit -> r) ->
  SomeBV bv ->
  r
unarySomeBV f _ (SomeBV bv) = f bv
unarySomeBV _ g (SomeBVLit i) = g i
{-# INLINE unarySomeBV #-}

-- | Lift a unary operation on sized bitvectors that returns a bitvector to
-- t'SomeBV'. The result will also be wrapped with t'SomeBV'.
unarySomeBVR1 ::
  (forall n. (KnownNat n, 1 <= n) => bv n -> bv n) ->
  (SomeBVLit -> SomeBVLit) ->
  SomeBV bv ->
  SomeBV bv
unarySomeBVR1 f g = unarySomeBV (SomeBV . f) (SomeBVLit . g)
{-# INLINE unarySomeBVR1 #-}

-- | Lift a binary operation on sized bitvectors that returns anything to
-- t'SomeBV'. Crash if the bitwidths do not match.
binSomeBV ::
  (forall n. (KnownNat n, 1 <= n) => Num (bv n), MaySomeBV bv) =>
  (forall n. (KnownNat n, 1 <= n) => bv n -> bv n -> r) ->
  (SomeBVLit -> SomeBVLit -> r) ->
  SomeBV bv ->
  SomeBV bv ->
  r
binSomeBV f _ (SomeBV (l :: bv l)) (SomeBV (r :: bv r)) =
  case sameNat (Proxy @l) (Proxy @r) of
    Just Refl -> f l r
    Nothing -> throw BitwidthMismatch
binSomeBV f _ (SomeBV (l :: bv l)) (SomeBVLit r) = f l $ assignLitBitWidth r
binSomeBV f _ (SomeBVLit l) (SomeBV (r :: bv r)) = f (assignLitBitWidth l) r
binSomeBV _ g (SomeBVLit l) (SomeBVLit r) = g l r
{-# INLINE binSomeBV #-}

-- | Lift a ternary operation on sized bitvectors that returns anything to
-- t'SomeBV'. Crash if the bitwidths do not match.
ternSomeBV ::
  (forall n. (KnownNat n, 1 <= n) => Num (bv n), MaySomeBV bv) =>
  (forall n. (KnownNat n, 1 <= n) => bv n -> bv n -> bv n -> r) ->
  SomeBV bv ->
  SomeBV bv ->
  SomeBV bv ->
  r
ternSomeBV f (SomeBV (a :: bv a)) (SomeBV (b :: bv b)) (SomeBV (c :: bv c)) =
  case (sameNat (Proxy @a) (Proxy @b), sameNat (Proxy @a) (Proxy @c)) of
    (Just Refl, Just Refl) -> f a b c
    _ -> throw BitwidthMismatch
ternSomeBV f a b c =
  case assignBitWidth "ternSomeBV" (a, b, c) of
    Right (a', b', c') -> ternSomeBV f a' b' c'
    Left e -> throw e
{-# INLINE ternSomeBV #-}

-- | Lift a binary operation on sized bitvectors that returns a bitvector to
-- t'SomeBV'. The result will also be wrapped with t'SomeBV'. Crash if the
-- bitwidths do not match.
binSomeBVR1 ::
  (forall n. (KnownNat n, 1 <= n) => Num (bv n), MaySomeBV bv) =>
  (forall n. (KnownNat n, 1 <= n) => bv n -> bv n -> bv n) ->
  (SomeBVLit -> SomeBVLit -> SomeBVLit) ->
  SomeBV bv ->
  SomeBV bv ->
  SomeBV bv
binSomeBVR1 f g = binSomeBV (\a b -> SomeBV $ f a b) (\a b -> SomeBVLit $ g a b)
{-# INLINE binSomeBVR1 #-}

-- | Lift a binary operation on sized bitvectors that returns two bitvectors to
-- t'SomeBV'. The results will also be wrapped with t'SomeBV'. Crash if the
-- bitwidths do not match.
binSomeBVR2 ::
  (forall n. (KnownNat n, 1 <= n) => Num (bv n), MaySomeBV bv) =>
  (forall n. (KnownNat n, 1 <= n) => bv n -> bv n -> (bv n, bv n)) ->
  (SomeBVLit -> SomeBVLit -> (SomeBVLit, SomeBVLit)) ->
  SomeBV bv ->
  SomeBV bv ->
  (SomeBV bv, SomeBV bv)
binSomeBVR2 f g =
  binSomeBV
    (\a b -> let (x, y) = f a b in (SomeBV x, SomeBV y))
    (\a b -> let (x, y) = g a b in (SomeBVLit x, SomeBVLit y))
{-# INLINE binSomeBVR2 #-}

-- | Lift a ternary operation on sized bitvectors that returns a bitvector to
-- t'SomeBV'. The result will also be wrapped with t'SomeBV'. Crash if the
-- bitwidths do not match.
ternSomeBVR1 ::
  (forall n. (KnownNat n, 1 <= n) => Num (bv n), MaySomeBV bv) =>
  (forall n. (KnownNat n, 1 <= n) => bv n -> bv n -> bv n -> bv n) ->
  SomeBV bv ->
  SomeBV bv ->
  SomeBV bv ->
  SomeBV bv
ternSomeBVR1 f = ternSomeBV (\a b c -> SomeBV $ f a b c)
{-# INLINE ternSomeBVR1 #-}

-- | Lift a binary operation on sized bitvectors that returns anything wrapped
-- with 'ExceptT' to t'SomeBV'. If the bitwidths do not match, throw an
-- 'BitwidthMismatch' error to the monadic context.
binSomeBVSafe ::
  ( MonadError (Either SomeBVException e) m,
    TryMerge m,
    Mergeable e,
    Mergeable r,
    forall n. (KnownNat n, 1 <= n) => Num (bv n),
    MaySomeBV bv
  ) =>
  (forall n. (KnownNat n, 1 <= n) => bv n -> bv n -> ExceptT e m r) ->
  (SomeBVLit -> SomeBVLit -> ExceptT (Either SomeBVException e) m r) ->
  SomeBV bv ->
  SomeBV bv ->
  m r
binSomeBVSafe f _ (SomeBV (l :: bv l)) (SomeBV (r :: bv r)) =
  case sameNat (Proxy @l) (Proxy @r) of
    Just Refl ->
      tryMerge $ runExceptT (f l r) >>= either (throwError . Right) pure
    Nothing -> tryMerge $ throwError $ Left BitwidthMismatch
binSomeBVSafe _ g (SomeBVLit l) (SomeBVLit r) =
  tryMerge $ runExceptT (g l r) >>= either throwError pure
binSomeBVSafe f g l r =
  case assignBitWidth "binSomeBVSafe" (l, r) of
    Right (l', r') -> binSomeBVSafe f g l' r'
    Left e -> tryMerge $ throwError $ Left e
{-# INLINE binSomeBVSafe #-}

-- | Lift a binary operation on sized bitvectors that returns a bitvector
-- wrapped with 'ExceptT' to t'SomeBV'. The result will also be wrapped with
-- t'SomeBV'.
--
-- If the bitwidths do not match, throw an 'BitwidthMismatch' error to the
-- monadic context.
binSomeBVSafeR1 ::
  ( MonadError (Either SomeBVException e) m,
    TryMerge m,
    Mergeable e,
    forall n. (KnownNat n, 1 <= n) => Mergeable (bv n),
    forall n. (KnownNat n, 1 <= n) => Num (bv n),
    MaySomeBV bv
  ) =>
  (forall n. (KnownNat n, 1 <= n) => bv n -> bv n -> ExceptT e m (bv n)) ->
  (SomeBVLit -> SomeBVLit -> ExceptT (Either SomeBVException e) m SomeBVLit) ->
  SomeBV bv ->
  SomeBV bv ->
  m (SomeBV bv)
binSomeBVSafeR1 f g =
  binSomeBVSafe
    (\l r -> mrgFmap SomeBV $ f l r)
    (\l r -> mrgFmap SomeBVLit $ g l r)
{-# INLINE binSomeBVSafeR1 #-}

-- | Lift a binary operation on sized bitvectors that returns two bitvectors
-- wrapped with 'ExceptT' to t'SomeBV'. The results will also be wrapped with
-- t'SomeBV'.
--
-- If the bitwidths do not match, throw an 'BitwidthMismatch' error to the
-- monadic context.
binSomeBVSafeR2 ::
  ( MonadError (Either SomeBVException e) m,
    TryMerge m,
    Mergeable e,
    forall n. (KnownNat n, 1 <= n) => Mergeable (bv n),
    forall n. (KnownNat n, 1 <= n) => Num (bv n),
    MaySomeBV bv
  ) =>
  ( forall n.
    (KnownNat n, 1 <= n) =>
    bv n ->
    bv n ->
    ExceptT e m (bv n, bv n)
  ) ->
  ( SomeBVLit ->
    SomeBVLit ->
    ExceptT (Either SomeBVException e) m (SomeBVLit, SomeBVLit)
  ) ->
  SomeBV bv ->
  SomeBV bv ->
  m (SomeBV bv, SomeBV bv)
binSomeBVSafeR2 f g =
  binSomeBVSafe
    (\l r -> mrgFmap (bimap SomeBV SomeBV) $ f l r)
    (\l r -> mrgFmap (bimap SomeBVLit SomeBVLit) $ g l r)
{-# INLINE binSomeBVSafeR2 #-}
