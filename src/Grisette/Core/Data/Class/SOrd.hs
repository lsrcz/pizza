{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Core.Data.Class.SOrd
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Core.Data.Class.SOrd
  ( -- * Symbolic total order relation
    SOrd (..),
    SOrd' (..),
  )
where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Writer.Lazy as WriterLazy
import qualified Control.Monad.Writer.Strict as WriterStrict
import qualified Data.ByteString as B
import Data.Functor.Sum
import Data.Int
import Data.Word
import GHC.TypeLits
import Generics.Deriving
import {-# SOURCE #-} Grisette.Core.Control.Monad.UnionM
import Grisette.Core.Data.BV
import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.SimpleMergeable
import Grisette.Core.Data.Class.Solvable
import {-# SOURCE #-} Grisette.IR.SymPrim.Data.SymPrim

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim
-- >>> :set -XDataKinds
-- >>> :set -XBinaryLiterals
-- >>> :set -XFlexibleContexts
-- >>> :set -XFlexibleInstances
-- >>> :set -XFunctionalDependencies

-- | Auxiliary class for 'SOrd' instance derivation
class (SEq' f) => SOrd' f where
  -- | Auxiliary function for '(<~~) derivation
  (<~~) :: f a -> f a -> SymBool

  infix 4 <~~

  -- | Auxiliary function for '(<=~~) derivation
  (<=~~) :: f a -> f a -> SymBool

  infix 4 <=~~

  -- | Auxiliary function for '(>~~) derivation
  (>~~) :: f a -> f a -> SymBool

  infix 4 >~~

  -- | Auxiliary function for '(>=~~) derivation
  (>=~~) :: f a -> f a -> SymBool

  infix 4 >=~~

  -- | Auxiliary function for 'symCompare' derivation
  symCompare' :: f a -> f a -> UnionM Ordering

instance SOrd' U1 where
  _ <~~ _ = con False
  _ <=~~ _ = con True
  _ >~~ _ = con False
  _ >=~~ _ = con True
  symCompare' _ _ = mrgSingle EQ

instance SOrd' V1 where
  _ <~~ _ = con False
  _ <=~~ _ = con True
  _ >~~ _ = con False
  _ >=~~ _ = con True
  symCompare' _ _ = mrgSingle EQ

instance (SOrd c) => SOrd' (K1 i c) where
  (K1 a) <~~ (K1 b) = a <~ b
  (K1 a) <=~~ (K1 b) = a <=~ b
  (K1 a) >~~ (K1 b) = a >~ b
  (K1 a) >=~~ (K1 b) = a >=~ b
  symCompare' (K1 a) (K1 b) = symCompare a b

instance (SOrd' a) => SOrd' (M1 i c a) where
  (M1 a) <~~ (M1 b) = a <~~ b
  (M1 a) <=~~ (M1 b) = a <=~~ b
  (M1 a) >~~ (M1 b) = a >~~ b
  (M1 a) >=~~ (M1 b) = a >=~~ b
  symCompare' (M1 a) (M1 b) = symCompare' a b

instance (SOrd' a, SOrd' b) => SOrd' (a :+: b) where
  (L1 _) <~~ (R1 _) = con True
  (L1 a) <~~ (L1 b) = a <~~ b
  (R1 _) <~~ (L1 _) = con False
  (R1 a) <~~ (R1 b) = a <~~ b
  (L1 _) <=~~ (R1 _) = con True
  (L1 a) <=~~ (L1 b) = a <=~~ b
  (R1 _) <=~~ (L1 _) = con False
  (R1 a) <=~~ (R1 b) = a <=~~ b

  (L1 _) >~~ (R1 _) = con False
  (L1 a) >~~ (L1 b) = a >~~ b
  (R1 _) >~~ (L1 _) = con True
  (R1 a) >~~ (R1 b) = a >~~ b
  (L1 _) >=~~ (R1 _) = con False
  (L1 a) >=~~ (L1 b) = a >=~~ b
  (R1 _) >=~~ (L1 _) = con True
  (R1 a) >=~~ (R1 b) = a >=~~ b

  symCompare' (L1 a) (L1 b) = symCompare' a b
  symCompare' (L1 _) (R1 _) = mrgSingle LT
  symCompare' (R1 a) (R1 b) = symCompare' a b
  symCompare' (R1 _) (L1 _) = mrgSingle GT

instance (SOrd' a, SOrd' b) => SOrd' (a :*: b) where
  (a1 :*: b1) <~~ (a2 :*: b2) = (a1 <~~ a2) ||~ ((a1 ==~~ a2) &&~ (b1 <~~ b2))
  (a1 :*: b1) <=~~ (a2 :*: b2) = (a1 <~~ a2) ||~ ((a1 ==~~ a2) &&~ (b1 <=~~ b2))
  (a1 :*: b1) >~~ (a2 :*: b2) = (a1 >~~ a2) ||~ ((a1 ==~~ a2) &&~ (b1 >~~ b2))
  (a1 :*: b1) >=~~ (a2 :*: b2) = (a1 >~~ a2) ||~ ((a1 ==~~ a2) &&~ (b1 >=~~ b2))
  symCompare' (a1 :*: b1) (a2 :*: b2) = do
    l <- symCompare' a1 a2
    case l of
      EQ -> symCompare' b1 b2
      _ -> mrgSingle l

derivedSymLt :: (Generic a, SOrd' (Rep a)) => a -> a -> SymBool
derivedSymLt x y = from x <~~ from y

derivedSymLe :: (Generic a, SOrd' (Rep a)) => a -> a -> SymBool
derivedSymLe x y = from x <=~~ from y

derivedSymGt :: (Generic a, SOrd' (Rep a)) => a -> a -> SymBool
derivedSymGt x y = from x >~~ from y

derivedSymGe :: (Generic a, SOrd' (Rep a)) => a -> a -> SymBool
derivedSymGe x y = from x >=~~ from y

derivedSymCompare :: (Generic a, SOrd' (Rep a)) => a -> a -> UnionM Ordering
derivedSymCompare x y = symCompare' (from x) (from y)

-- | Symbolic total order. Note that we can't use Haskell's 'Ord' class since
-- symbolic comparison won't necessarily return a concrete 'Bool' or 'Ordering'
-- value.
--
-- >>> let a = 1 :: SymInteger
-- >>> let b = 2 :: SymInteger
-- >>> a <~ b
-- true
-- >>> a >~ b
-- false
--
-- >>> let a = "a" :: SymInteger
-- >>> let b = "b" :: SymInteger
-- >>> a <~ b
-- (< a b)
-- >>> a <=~ b
-- (<= a b)
-- >>> a >~ b
-- (< b a)
-- >>> a >=~ b
-- (<= b a)
--
-- For `symCompare`, `Ordering` is not a solvable type, and the result would
-- be wrapped in a union-like monad. See `Grisette.Core.Control.Monad.UnionMBase` and `UnionLike` for more
-- information.
--
-- >>> a `symCompare` b :: UnionM Ordering -- UnionM is UnionMBase specialized with SymBool
-- {If (< a b) LT (If (= a b) EQ GT)}
--
-- __Note:__ This type class can be derived for algebraic data types.
-- You may need the @DerivingVia@ and @DerivingStrategies@ extensions.
--
-- > data X = ... deriving Generic deriving SOrd via (Default X)
class (SEq a) => SOrd a where
  (<~) :: a -> a -> SymBool
  infix 4 <~
  (<=~) :: a -> a -> SymBool
  infix 4 <=~
  (>~) :: a -> a -> SymBool
  infix 4 >~
  (>=~) :: a -> a -> SymBool
  infix 4 >=~
  x <~ y = x <=~ y &&~ x /=~ y
  x >~ y = y <~ x
  x >=~ y = y <=~ x
  symCompare :: a -> a -> UnionM Ordering
  symCompare l r =
    mrgIf
      (l <~ r)
      (mrgSingle LT)
      (mrgIf (l ==~ r) (mrgSingle EQ) (mrgSingle GT))
  {-# MINIMAL (<=~) #-}

instance (SEq a, Generic a, SOrd' (Rep a)) => SOrd (Default a) where
  (Default l) <=~ (Default r) = l `derivedSymLe` r
  (Default l) <~ (Default r) = l `derivedSymLt` r
  (Default l) >=~ (Default r) = l `derivedSymGe` r
  (Default l) >~ (Default r) = l `derivedSymGt` r
  symCompare (Default l) (Default r) = derivedSymCompare l r

#define CONCRETE_SORD(type) \
instance SOrd type where \
  l <=~ r = con $ l <= r; \
  l <~ r = con $ l < r; \
  l >=~ r = con $ l >= r; \
  l >~ r = con $ l > r; \
  symCompare l r = mrgSingle $ compare l r

#define CONCRETE_SORD_BV(type) \
instance (KnownNat n, 1 <= n) => SOrd (type n) where \
  l <=~ r = con $ l <= r; \
  l <~ r = con $ l < r; \
  l >=~ r = con $ l >= r; \
  l >~ r = con $ l > r; \
  symCompare l r = mrgSingle $ compare l r

#if 1
CONCRETE_SORD(Bool)
CONCRETE_SORD(Integer)
CONCRETE_SORD(Char)
CONCRETE_SORD(Int)
CONCRETE_SORD(Int8)
CONCRETE_SORD(Int16)
CONCRETE_SORD(Int32)
CONCRETE_SORD(Int64)
CONCRETE_SORD(Word)
CONCRETE_SORD(Word8)
CONCRETE_SORD(Word16)
CONCRETE_SORD(Word32)
CONCRETE_SORD(Word64)
CONCRETE_SORD(SomeWordN)
CONCRETE_SORD(SomeIntN)
CONCRETE_SORD(B.ByteString)
CONCRETE_SORD_BV(WordN)
CONCRETE_SORD_BV(IntN)
#endif

symCompareSingleList :: (SOrd a) => Bool -> Bool -> [a] -> [a] -> SymBool
symCompareSingleList isLess isStrict = go
  where
    go [] [] = con (not isStrict)
    go (x : xs) (y : ys) = (if isLess then x <~ y else x >~ y) ||~ (x ==~ y &&~ go xs ys)
    go [] _ = if isLess then con True else con False
    go _ [] = if isLess then con False else con True

symCompareList :: (SOrd a) => [a] -> [a] -> UnionM Ordering
symCompareList [] [] = mrgSingle EQ
symCompareList (x : xs) (y : ys) = do
  oxy <- symCompare x y
  case oxy of
    LT -> mrgSingle LT
    EQ -> symCompareList xs ys
    GT -> mrgSingle GT
symCompareList [] _ = mrgSingle LT
symCompareList _ [] = mrgSingle GT

instance (SOrd a) => SOrd [a] where
  (<=~) = symCompareSingleList True False
  (<~) = symCompareSingleList True True
  (>=~) = symCompareSingleList False False
  (>~) = symCompareSingleList False True
  symCompare = symCompareList

deriving via (Default (Maybe a)) instance SOrd a => SOrd (Maybe a)

deriving via (Default (Either a b)) instance (SOrd a, SOrd b) => SOrd (Either a b)

deriving via (Default ()) instance SOrd ()

deriving via (Default (a, b)) instance (SOrd a, SOrd b) => SOrd (a, b)

deriving via (Default (a, b, c)) instance (SOrd a, SOrd b, SOrd c) => SOrd (a, b, c)

deriving via
  (Default (a, b, c, d))
  instance
    (SOrd a, SOrd b, SOrd c, SOrd d) =>
    SOrd (a, b, c, d)

deriving via
  (Default (a, b, c, d, e))
  instance
    (SOrd a, SOrd b, SOrd c, SOrd d, SOrd e) =>
    SOrd (a, b, c, d, e)

deriving via
  (Default (a, b, c, d, e, f))
  instance
    (SOrd a, SOrd b, SOrd c, SOrd d, SOrd e, SOrd f) =>
    SOrd (a, b, c, d, e, f)

deriving via
  (Default (a, b, c, d, e, f, g))
  instance
    (SOrd a, SOrd b, SOrd c, SOrd d, SOrd e, SOrd f, SOrd g) =>
    SOrd (a, b, c, d, e, f, g)

deriving via
  (Default (a, b, c, d, e, f, g, h))
  instance
    ( SOrd a,
      SOrd b,
      SOrd c,
      SOrd d,
      SOrd e,
      SOrd f,
      SOrd g,
      SOrd h
    ) =>
    SOrd (a, b, c, d, e, f, g, h)

deriving via
  (Default (Sum f g a))
  instance
    (SOrd (f a), SOrd (g a)) => SOrd (Sum f g a)

instance (SOrd (m (Maybe a))) => SOrd (MaybeT m a) where
  (MaybeT l) <=~ (MaybeT r) = l <=~ r
  (MaybeT l) <~ (MaybeT r) = l <~ r
  (MaybeT l) >=~ (MaybeT r) = l >=~ r
  (MaybeT l) >~ (MaybeT r) = l >~ r
  symCompare (MaybeT l) (MaybeT r) = symCompare l r

instance (SOrd (m (Either e a))) => SOrd (ExceptT e m a) where
  (ExceptT l) <=~ (ExceptT r) = l <=~ r
  (ExceptT l) <~ (ExceptT r) = l <~ r
  (ExceptT l) >=~ (ExceptT r) = l >=~ r
  (ExceptT l) >~ (ExceptT r) = l >~ r
  symCompare (ExceptT l) (ExceptT r) = symCompare l r

instance (SOrd (m (a, s))) => SOrd (WriterLazy.WriterT s m a) where
  (WriterLazy.WriterT l) <=~ (WriterLazy.WriterT r) = l <=~ r
  (WriterLazy.WriterT l) <~ (WriterLazy.WriterT r) = l <~ r
  (WriterLazy.WriterT l) >=~ (WriterLazy.WriterT r) = l >=~ r
  (WriterLazy.WriterT l) >~ (WriterLazy.WriterT r) = l >~ r
  symCompare (WriterLazy.WriterT l) (WriterLazy.WriterT r) = symCompare l r

instance (SOrd (m (a, s))) => SOrd (WriterStrict.WriterT s m a) where
  (WriterStrict.WriterT l) <=~ (WriterStrict.WriterT r) = l <=~ r
  (WriterStrict.WriterT l) <~ (WriterStrict.WriterT r) = l <~ r
  (WriterStrict.WriterT l) >=~ (WriterStrict.WriterT r) = l >=~ r
  (WriterStrict.WriterT l) >~ (WriterStrict.WriterT r) = l >~ r
  symCompare (WriterStrict.WriterT l) (WriterStrict.WriterT r) = symCompare l r

instance (SOrd a) => SOrd (Identity a) where
  (Identity l) <=~ (Identity r) = l <=~ r
  (Identity l) <~ (Identity r) = l <~ r
  (Identity l) >=~ (Identity r) = l >=~ r
  (Identity l) >~ (Identity r) = l >~ r
  (Identity l) `symCompare` (Identity r) = l `symCompare` r

instance (SOrd (m a)) => SOrd (IdentityT m a) where
  (IdentityT l) <=~ (IdentityT r) = l <=~ r
  (IdentityT l) <~ (IdentityT r) = l <~ r
  (IdentityT l) >=~ (IdentityT r) = l >=~ r
  (IdentityT l) >~ (IdentityT r) = l >~ r
  (IdentityT l) `symCompare` (IdentityT r) = l `symCompare` r
