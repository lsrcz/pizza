{-# LANGUAGE FlexibleContexts #-}

module Pizza.TestUtils.ExtractSymbolics where

import qualified Data.HashSet as S
import Pizza.Core.Data.Class.ExtractSymbolics
import Pizza.TestUtils.SBool
import Test.Tasty.HUnit

concreteExtractSymbolicsOkProp :: (HasCallStack, ExtractSymbolics (S.HashSet Symbol) a) => (a, a) -> Assertion
concreteExtractSymbolicsOkProp x = extractSymbolics x @=? (S.empty :: S.HashSet Symbol)
