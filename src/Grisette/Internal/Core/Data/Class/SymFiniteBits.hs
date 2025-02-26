{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Internal.Core.Data.Class.SymFiniteBits
-- Copyright   :   (c) Sirui Lu 2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.Core.Data.Class.SymFiniteBits
  ( lsb,
    msb,
    setBitTo,
    bitBlast,
    FromBits (..),
    SymFiniteBits (..),
    symBitBlast,
    symLsb,
    symMsb,
    symPopCount,
    symCountLeadingZeros,
    symCountTrailingZeros,
  )
where

import Data.Bits
  ( Bits (bit, clearBit, setBit, testBit, zeroBits, (.|.)),
    FiniteBits (finiteBitSize),
  )
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.TypeLits (KnownNat, type (<=))
import Grisette.Internal.Core.Data.Class.BitVector
  ( BV (bv, bvConcat, bvSelect),
  )
import Grisette.Internal.Core.Data.Class.ITEOp (ITEOp (symIte))
import Grisette.Internal.Core.Data.Class.SymEq (SymEq ((.==)))
import Grisette.Internal.SymPrim.BV (IntN, WordN)
import Grisette.Internal.SymPrim.SomeBV
  ( SomeBV (SomeBV),
    SomeIntN,
    SomeWordN,
    unsafeSomeBV,
  )
import Grisette.Internal.SymPrim.SymBV (SymIntN, SymWordN)
import Grisette.Internal.SymPrim.SymBool (SymBool)
import Unsafe.Coerce (unsafeCoerce)

-- | Set a bit in a concrete value to a specific value.
setBitTo :: (Bits a) => a -> Int -> Bool -> a
setBitTo v i b = if b then setBit v i else clearBit v i

-- | Bit-blast a concrete value into a list of concrete bits. The first element
-- in the resulting list corresponds to the least significant bit.
bitBlast :: (FiniteBits a) => a -> [Bool]
bitBlast x = map (testBit x) [0 .. finiteBitSize x - 1]

-- | Extract the least significant bit of a concrete value.
lsb :: (Bits a) => a -> Bool
lsb x = testBit x 0

-- | Extract the most significant bit of a concrete value.
msb :: (FiniteBits a) => a -> Bool
msb x = testBit x (finiteBitSize x - 1)

-- | Type class for assembling concrete bits to a bit-vector.
class (FiniteBits a) => FromBits a where
  -- | Assembling concrete bits to a bit-vector. The first boolean value in the
  -- list corresponding to the least signification value.
  fromBits :: [Bool] -> a
  fromBits bits
    | length bits /= finiteBitSize (undefined :: a) =
        error "fromBits: length mismatch"
    | otherwise = foldl1 (.|.) lst
    where
      lst :: [a]
      lst = (\(pos, b) -> if b then bit pos else zeroBits) <$> zip [0 ..] bits

instance FromBits Int

instance FromBits Int8

instance FromBits Int16

instance FromBits Int32

instance FromBits Int64

instance FromBits Word

instance FromBits Word8

instance FromBits Word16

instance FromBits Word32

instance FromBits Word64

instance (KnownNat n, 1 <= n) => FromBits (WordN n)

instance (KnownNat n, 1 <= n) => FromBits (IntN n)

instance FromBits SomeIntN where
  fromBits bits
    | null bits =
        error "Cannot create a SomeBV from an empty list of bits."
  fromBits bits = unsafeSomeBV (length bits) $ \_ -> fromBits bits

instance FromBits SomeWordN where
  fromBits bits
    | null bits =
        error "Cannot create a SomeBV from an empty list of bits."
  fromBits bits = unsafeSomeBV (length bits) $ \_ -> fromBits bits

-- | A class for symbolic finite bit operations.
class (FiniteBits a, ITEOp a) => SymFiniteBits a where
  -- | Test a symbolic bit in a symbolic bit-vector.
  symTestBit :: a -> Int -> SymBool

  -- | Set a bit in a symbolic value to a specific value.
  symSetBitTo :: a -> Int -> SymBool -> a
  symSetBitTo v i b = symIte b (setBit v i) (clearBit v i)

  -- | Assembling symbolic bits to a symbolic bit-vector. The first symbolic
  -- boolean value in the list corresponding to the least signification value.
  symFromBits :: [SymBool] -> a

instance SymFiniteBits (SomeBV SymIntN) where
  symTestBit v i = bvSelect i 1 v .== bv 1 1
  symFromBits bits
    | null bits =
        error "Cannot create a SomeBV from an empty list of bits."
  symFromBits bits = unsafeSomeBV (length bits) $ \_ -> symFromBits bits

instance SymFiniteBits (SomeBV SymWordN) where
  symTestBit v i = bvSelect i 1 v .== bv 1 1
  symFromBits bits
    | null bits =
        error "Cannot create a SomeBV from an empty list of bits."
  symFromBits bits = unsafeSomeBV (length bits) $ \_ -> symFromBits bits

instance (KnownNat n, 1 <= n) => SymFiniteBits (SymIntN n) where
  symTestBit v = symTestBit (SomeBV v)
  symSetBitTo v i b = symIte b (setBit v i) (clearBit v i)
  symFromBits bits
    | length bits /= finiteBitSize (undefined :: SymWordN n) =
        error "symFromBits: length mismatch"
    | otherwise = case foldl1 bvConcat $ SomeBV <$> reverse lst of
        SomeBV r -> unsafeCoerce r
        _ -> error "symFromBits: length mismatch"
    where
      lst :: [SymIntN 1]
      lst = (\b -> symIte b 1 0) <$> bits

instance (KnownNat n, 1 <= n) => SymFiniteBits (SymWordN n) where
  symTestBit v = symTestBit (SomeBV v)
  symSetBitTo v i b = symIte b (setBit v i) (clearBit v i)
  symFromBits bits
    | length bits /= finiteBitSize (undefined :: SymWordN n) =
        error "symFromBits: length mismatch"
    | otherwise = case foldl1 bvConcat $ SomeBV <$> reverse lst of
        SomeBV r -> unsafeCoerce r
        _ -> error "symFromBits: length mismatch"
    where
      lst :: [SymWordN 1]
      lst = (\b -> symIte b 1 0) <$> bits

-- | Bit-blast a symbolic value into a list of symbolic bits. The first element
-- in the resulting list corresponds to the least significant bit.
symBitBlast :: (SymFiniteBits a) => a -> [SymBool]
symBitBlast x = map (symTestBit x) [0 .. finiteBitSize x - 1]

-- | Extract the least significant bit of a symbolic value.
symLsb :: (SymFiniteBits a) => a -> SymBool
symLsb x = symTestBit x 0

-- | Extract the most significant bit of a symbolic value.
symMsb :: (SymFiniteBits a) => a -> SymBool
symMsb x = symTestBit x (finiteBitSize x - 1)

-- | Count the number of set bits in a symbolic value.
symPopCount :: (Num a, ITEOp a, SymFiniteBits a) => a -> a
symPopCount v = 0 * v + adderTree (map fromBool (symBitBlast v))
  where
    -- Convert a symbolic boolean to a numeric value (0 or 1)
    -- Node: 0 * v + is a trick to assign the correct bit-width to the result.
    fromBool b = 0 * v + symIte b 1 0

    -- Implement binary adder tree to efficiently sum values
    adderTree :: (Num a) => [a] -> a
    adderTree [] = 0
    adderTree [x] = x
    adderTree xs = adderTree (pairwiseAdd xs)

    -- Add adjacent pairs of values
    pairwiseAdd :: (Num a) => [a] -> [a]
    pairwiseAdd [] = []
    pairwiseAdd [x] = [x]
    pairwiseAdd (x : y : rest) = (x + y) : pairwiseAdd rest

-- | Count the number of leading zeros in a symbolic value.
symCountLeadingZeros :: (Num a, ITEOp a, SymFiniteBits a) => a -> a
-- Node: 0 * v + is a trick to assign the correct bit-width to the result.
symCountLeadingZeros v = 0 * v + go bits rs
  where
    bits = reverse $ symBitBlast v
    rs = fromIntegral <$> [0 ..]
    go [] (r : _) = r
    go (b : bs) (r : rs) = symIte b r (go bs rs)
    go _ [] = error "Should not happen"

-- | Count the number of trailing zeros in a symbolic value.
symCountTrailingZeros :: (Num a, ITEOp a, SymFiniteBits a) => a -> a
-- Node: 0 * v + is a trick to assign the correct bit-width to the result.
symCountTrailingZeros v = 0 * v + go bits rs
  where
    bits = symBitBlast v
    rs = fromIntegral <$> [0 ..]
    go [] (r : _) = r
    go (b : bs) (r : rs) = symIte b r (go bs rs)
    go _ [] = error "Should not happen"
