{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Grisette.SymPrim.Prim.BitsTests (bitsTests) where

import Data.Bits (Bits (rotateL, rotateR), FiniteBits)
import Data.Data (Proxy (Proxy))
import Grisette (IntN, SupportedPrim, WordN)
import Grisette.Internal.SymPrim.Prim.Term
  ( PEvalBitwiseTerm
      ( pevalAndBitsTerm,
        pevalComplementBitsTerm,
        pevalOrBitsTerm,
        pevalXorBitsTerm
      ),
    PEvalRotateTerm
      ( pevalRotateLeftTerm,
        pevalRotateRightTerm
      ),
    PEvalShiftTerm
      ( pevalShiftLeftTerm,
        pevalShiftRightTerm
      ),
    Term,
    andBitsTerm,
    bvConcatTerm,
    bvExtendTerm,
    bvSelectTerm,
    complementBitsTerm,
    conTerm,
    orBitsTerm,
    rotateLeftTerm,
    rotateRightTerm,
    shiftLeftTerm,
    shiftRightTerm,
    ssymTerm,
    xorBitsTerm,
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit ((@?=))
import Test.QuickCheck (Property, discard, ioProperty)

bitsTests :: Test
bitsTests =
  testGroup
    "Bits"
    [ testGroup
        "AndBits"
        [ testCase "On both concrete" $ do
            pevalAndBitsTerm
              (conTerm 3 :: Term (WordN 4))
              (conTerm 5)
              @?= conTerm 1,
          testCase "On zeroBits" $ do
            pevalAndBitsTerm
              (conTerm 0 :: Term (WordN 4))
              (ssymTerm "a")
              @?= conTerm 0
            pevalAndBitsTerm
              (ssymTerm "a")
              (conTerm 0 :: Term (WordN 4))
              @?= conTerm 0,
          testCase "On all one bits" $ do
            pevalAndBitsTerm
              (conTerm 15 :: Term (WordN 4))
              (ssymTerm "a")
              @?= ssymTerm "a"
            pevalAndBitsTerm
              (ssymTerm "a")
              (conTerm 15 :: Term (WordN 4))
              @?= ssymTerm "a",
          testCase "On symbolic" $ do
            pevalAndBitsTerm
              (ssymTerm "a" :: Term (WordN 4))
              (ssymTerm "b")
              @?= andBitsTerm
                (ssymTerm "a" :: Term (WordN 4))
                (ssymTerm "b" :: Term (WordN 4))
        ],
      testGroup
        "OrBits"
        [ testCase "On both concrete" $ do
            pevalOrBitsTerm
              (conTerm 3 :: Term (WordN 4))
              (conTerm 5)
              @?= conTerm 7,
          testCase "On zeroBits" $ do
            pevalOrBitsTerm
              (conTerm 0 :: Term (WordN 4))
              (ssymTerm "a")
              @?= ssymTerm "a"
            pevalOrBitsTerm
              (ssymTerm "a")
              (conTerm 0 :: Term (WordN 4))
              @?= ssymTerm "a",
          testCase "On all one bits" $ do
            pevalOrBitsTerm
              (conTerm 15 :: Term (WordN 4))
              (ssymTerm "a")
              @?= conTerm 15
            pevalOrBitsTerm
              (ssymTerm "a")
              (conTerm 15 :: Term (WordN 4))
              @?= conTerm 15,
          testCase "On symbolic" $ do
            pevalOrBitsTerm
              (ssymTerm "a" :: Term (WordN 4))
              (ssymTerm "b")
              @?= orBitsTerm
                (ssymTerm "a" :: Term (WordN 4))
                (ssymTerm "b" :: Term (WordN 4))
        ],
      testGroup
        "XorBits"
        [ testCase "On both concrete" $ do
            pevalXorBitsTerm
              (conTerm 3 :: Term (WordN 4))
              (conTerm 5)
              @?= conTerm 6,
          testCase "On zeroBits" $ do
            pevalXorBitsTerm
              (conTerm 0 :: Term (WordN 4))
              (ssymTerm "a")
              @?= ssymTerm "a"
            pevalXorBitsTerm
              (ssymTerm "a")
              (conTerm 0 :: Term (WordN 4))
              @?= ssymTerm "a",
          testCase "On all one bits" $ do
            pevalXorBitsTerm
              (conTerm 15 :: Term (WordN 4))
              (ssymTerm "a")
              @?= pevalComplementBitsTerm (ssymTerm "a")
            pevalXorBitsTerm
              (ssymTerm "a")
              (conTerm 15 :: Term (WordN 4))
              @?= pevalComplementBitsTerm (ssymTerm "a"),
          testCase "On single complement" $ do
            pevalXorBitsTerm
              (pevalComplementBitsTerm $ ssymTerm "a" :: Term (WordN 4))
              (ssymTerm "b")
              @?= pevalComplementBitsTerm (pevalXorBitsTerm (ssymTerm "a") (ssymTerm "b"))
            pevalXorBitsTerm
              (ssymTerm "a" :: Term (WordN 4))
              (pevalComplementBitsTerm $ ssymTerm "b")
              @?= pevalComplementBitsTerm (pevalXorBitsTerm (ssymTerm "a") (ssymTerm "b")),
          testCase "On both complement" $ do
            pevalXorBitsTerm
              (pevalComplementBitsTerm $ ssymTerm "a" :: Term (WordN 4))
              (pevalComplementBitsTerm $ ssymTerm "b")
              @?= pevalXorBitsTerm (ssymTerm "a") (ssymTerm "b"),
          testCase "On symbolic" $ do
            pevalXorBitsTerm
              (ssymTerm "a" :: Term (WordN 4))
              (ssymTerm "b")
              @?= xorBitsTerm
                (ssymTerm "a" :: Term (WordN 4))
                (ssymTerm "b" :: Term (WordN 4))
        ],
      testGroup
        "ComplementBits"
        [ testCase "On concrete" $ do
            pevalComplementBitsTerm (conTerm 5 :: Term (WordN 4)) @?= conTerm 10,
          testCase "On complement" $ do
            pevalComplementBitsTerm (pevalComplementBitsTerm (ssymTerm "a") :: Term (WordN 4)) @?= ssymTerm "a",
          testCase "On symbolic" $ do
            pevalComplementBitsTerm (ssymTerm "a" :: Term (WordN 4))
              @?= complementBitsTerm (ssymTerm "a" :: Term (WordN 4))
        ],
      testGroup
        "ShiftLeft"
        [ testCase "On concrete" $ do
            pevalShiftLeftTerm (conTerm 15 :: Term (WordN 4)) (conTerm 2) @?= conTerm 12
            pevalShiftLeftTerm (conTerm 15 :: Term (IntN 4)) (conTerm 2) @?= conTerm 12,
          testCase "shift 0" $ do
            pevalShiftLeftTerm (ssymTerm "a" :: Term (WordN 4)) (conTerm 0) @?= ssymTerm "a"
            pevalShiftLeftTerm (ssymTerm "a" :: Term (IntN 4)) (conTerm 0) @?= ssymTerm "a",
          testCase "shift greater or equal to left bitsize" $ do
            pevalShiftLeftTerm (ssymTerm "a" :: Term (WordN 4)) (conTerm 4) @?= conTerm 0
            pevalShiftLeftTerm (ssymTerm "a" :: Term (IntN 4)) (conTerm 4) @?= conTerm 0
            pevalShiftLeftTerm (ssymTerm "a" :: Term (WordN 4)) (conTerm 5) @?= conTerm 0
            pevalShiftLeftTerm (ssymTerm "a" :: Term (IntN 4)) (conTerm 5) @?= conTerm 0,
          testCase "shift negative amount is undefined on for IntN" $ do
            pevalShiftLeftTerm (conTerm 15 :: Term (IntN 4)) (conTerm $ -1)
              @?= shiftLeftTerm (conTerm 15) (conTerm $ -1)
            pevalShiftLeftTerm (conTerm 15 :: Term (IntN 4)) (conTerm $ -8)
              @?= shiftLeftTerm (conTerm 15) (conTerm $ -8),
          testCase "shift symbolic" $ do
            pevalShiftLeftTerm (ssymTerm "a" :: Term (WordN 4)) (ssymTerm "b")
              @?= shiftLeftTerm (ssymTerm "a" :: Term (WordN 4)) (ssymTerm "b"),
          testCase "shift symbolic by concrete" $ do
            pevalShiftLeftTerm (ssymTerm "a" :: Term (WordN 4)) (conTerm 2)
              @?= bvConcatTerm
                (bvSelectTerm (Proxy @0) (Proxy @2) (ssymTerm "a" :: Term (WordN 4)))
                (conTerm 0 :: Term (WordN 2)),
          testCase "Regression: shift by very large number" $ do
            pevalShiftLeftTerm (conTerm 15 :: Term (IntN 128)) (conTerm maxBound) @?= conTerm 0
            pevalShiftLeftTerm (conTerm 15 :: Term (WordN 128)) (conTerm maxBound) @?= conTerm 0
        ],
      testGroup
        "ShiftRight"
        [ testCase "On concrete, should perform arithmetic shifting on IntN" $ do
            pevalShiftRightTerm (conTerm 7 :: Term (IntN 4)) (conTerm 2) @?= conTerm 1
            pevalShiftRightTerm (conTerm 15 :: Term (IntN 4)) (conTerm 2) @?= conTerm 15,
          testCase "On concrete, should perform logical shifting on WordN" $ do
            pevalShiftRightTerm (conTerm 7 :: Term (WordN 4)) (conTerm 2) @?= conTerm 1
            pevalShiftRightTerm (conTerm 15 :: Term (WordN 4)) (conTerm 2) @?= conTerm 3,
          testCase "shift 0" $ do
            pevalShiftRightTerm (ssymTerm "a" :: Term (WordN 4)) (conTerm 0) @?= ssymTerm "a"
            pevalShiftRightTerm (ssymTerm "a" :: Term (IntN 4)) (conTerm 0) @?= ssymTerm "a",
          testCase "shift greater or equal to left bitsize on WordN" $ do
            pevalShiftRightTerm (ssymTerm "a" :: Term (WordN 4)) (conTerm 4) @?= conTerm 0
            pevalShiftRightTerm (ssymTerm "a" :: Term (WordN 4)) (conTerm 5) @?= conTerm 0,
          testCase "shift greater or equal to left bitsize on IntN" $ do
            pevalShiftRightTerm (ssymTerm "a" :: Term (IntN 4)) (conTerm 5)
              @?= bvExtendTerm
                True
                (Proxy @4)
                ( bvSelectTerm
                    (Proxy @3)
                    (Proxy @1)
                    (ssymTerm "a" :: Term (IntN 4))
                )
            pevalShiftRightTerm (ssymTerm "a" :: Term (IntN 4)) (conTerm 4)
              @?= bvExtendTerm
                True
                (Proxy @4)
                ( bvSelectTerm
                    (Proxy @3)
                    (Proxy @1)
                    (ssymTerm "a" :: Term (IntN 4))
                ),
          testCase "shift negative amount is undefined on for IntN" $ do
            pevalShiftRightTerm (conTerm 15 :: Term (IntN 4)) (conTerm $ -1)
              @?= shiftRightTerm (conTerm 15) (conTerm $ -1)
            pevalShiftRightTerm (conTerm 15 :: Term (IntN 4)) (conTerm $ -8)
              @?= shiftRightTerm (conTerm 15) (conTerm $ -8),
          testCase "shift symbolic" $ do
            pevalShiftRightTerm (ssymTerm "a" :: Term (WordN 4)) (ssymTerm "b")
              @?= shiftRightTerm (ssymTerm "a" :: Term (WordN 4)) (ssymTerm "b"),
          testCase "shift symbolic by concrete" $ do
            pevalShiftRightTerm (ssymTerm "a" :: Term (WordN 4)) (conTerm 2)
              @?= bvConcatTerm
                (conTerm 0 :: Term (WordN 2))
                (bvSelectTerm (Proxy @2) (Proxy @2) (ssymTerm "a" :: Term (WordN 4)))
            pevalShiftRightTerm (ssymTerm "a" :: Term (IntN 4)) (conTerm 2)
              @?= bvExtendTerm
                True
                (Proxy @4)
                (bvSelectTerm (Proxy @2) (Proxy @2) (ssymTerm "a" :: Term (IntN 4))),
          testCase "Regression: shift by very large number" $ do
            pevalShiftRightTerm (conTerm 15 :: Term (IntN 128)) (conTerm maxBound) @?= conTerm 0
            pevalShiftRightTerm (conTerm 15 :: Term (WordN 128)) (conTerm maxBound) @?= conTerm 0
        ],
      testGroup
        "RotateLeft"
        [ testCase "On concrete" $ do
            pevalRotateLeftTerm (conTerm 0b10100101 :: Term (WordN 8)) (conTerm 2) @?= conTerm 0b10010110
            pevalRotateLeftTerm (conTerm 0b10100101 :: Term (IntN 8)) (conTerm 2) @?= conTerm 0b10010110,
          testCase "rotate 0" $ do
            pevalRotateLeftTerm (ssymTerm "a" :: Term (WordN 4)) (conTerm 0) @?= ssymTerm "a"
            pevalRotateLeftTerm (ssymTerm "a" :: Term (IntN 4)) (conTerm 0) @?= ssymTerm "a",
          testCase "rotate bitsize" $ do
            pevalRotateLeftTerm (ssymTerm "a" :: Term (WordN 4)) (conTerm 4)
              @?= ssymTerm "a"
            pevalRotateLeftTerm (ssymTerm "a" :: Term (IntN 4)) (conTerm 4)
              @?= ssymTerm "a",
          testCase "rotate greater than left bitsize" $ do
            pevalRotateLeftTerm (ssymTerm "a" :: Term (WordN 4)) (conTerm 5)
              @?= rotateLeftTerm (ssymTerm "a" :: Term (WordN 4)) (conTerm 1)
            pevalRotateLeftTerm (ssymTerm "a" :: Term (IntN 4)) (conTerm 5)
              @?= rotateLeftTerm (ssymTerm "a" :: Term (IntN 4)) (conTerm 1),
          testCase "rotate negative amount is undefined on for IntN" $ do
            pevalRotateLeftTerm (conTerm 15 :: Term (IntN 4)) (conTerm $ -1)
              @?= rotateLeftTerm (conTerm 15) (conTerm $ -1)
            pevalRotateLeftTerm (conTerm 15 :: Term (IntN 4)) (conTerm $ -8)
              @?= rotateLeftTerm (conTerm 15) (conTerm $ -8),
          testCase "rotate symbolic" $ do
            pevalRotateLeftTerm (ssymTerm "a" :: Term (WordN 4)) (conTerm 2)
              @?= rotateLeftTerm (ssymTerm "a" :: Term (WordN 4)) (conTerm 2),
          testCase "Regression: rotate by very large number" $ do
            pevalRotateLeftTerm (conTerm 15 :: Term (IntN 128)) (conTerm maxBound) @?= conTerm (rotateR 15 1)
            pevalRotateLeftTerm (conTerm 15 :: Term (WordN 128)) (conTerm maxBound) @?= conTerm (rotateR 15 1)
        ],
      testGroup
        "RotateRight"
        [ testProperty "On concrete WordN 1" $
            concreteSmallRotateRightCorrect @(WordN 1),
          testProperty "On concrete WordN 2" $
            concreteSmallRotateRightCorrect @(WordN 2),
          testProperty "On concrete WordN 3" $
            concreteSmallRotateRightCorrect @(WordN 3),
          testProperty "On concrete WordN 4" $
            concreteSmallRotateRightCorrect @(WordN 4),
          testProperty "On concrete WordN 8" $
            concreteSmallRotateRightCorrect @(WordN 8),
          testProperty "On concrete IntN 1" $
            concreteSmallRotateRightCorrect @(IntN 1),
          testProperty "On concrete IntN 2" $
            concreteSmallRotateRightCorrect @(IntN 2),
          testProperty "On concrete IntN 3" $
            concreteSmallRotateRightCorrect @(IntN 3),
          testProperty "On concrete IntN 4" $
            concreteSmallRotateRightCorrect @(IntN 4),
          testProperty "On concrete IntN 8" $
            concreteSmallRotateRightCorrect @(IntN 8),
          testCase "rotate 0" $ do
            pevalRotateRightTerm (ssymTerm "a" :: Term (WordN 4)) (conTerm 0) @?= ssymTerm "a"
            pevalRotateRightTerm (ssymTerm "a" :: Term (IntN 4)) (conTerm 0) @?= ssymTerm "a",
          testCase "rotate bitsize" $ do
            pevalRotateRightTerm (ssymTerm "a" :: Term (WordN 4)) (conTerm 4)
              @?= ssymTerm "a"
            pevalRotateRightTerm (ssymTerm "a" :: Term (IntN 4)) (conTerm 4)
              @?= ssymTerm "a",
          testCase "rotate greater than left bitsize" $ do
            pevalRotateRightTerm (ssymTerm "a" :: Term (WordN 4)) (conTerm 5)
              @?= rotateRightTerm (ssymTerm "a" :: Term (WordN 4)) (conTerm 1)
            pevalRotateRightTerm (ssymTerm "a" :: Term (IntN 4)) (conTerm 5)
              @?= rotateRightTerm (ssymTerm "a" :: Term (IntN 4)) (conTerm 1),
          testCase "rotate negative amount is undefined on for IntN" $ do
            pevalRotateRightTerm (conTerm 15 :: Term (IntN 4)) (conTerm $ -1)
              @?= rotateRightTerm (conTerm 15) (conTerm $ -1)
            pevalRotateRightTerm (conTerm 15 :: Term (IntN 4)) (conTerm $ -8)
              @?= rotateRightTerm (conTerm 15) (conTerm $ -8),
          testCase "rotate symbolic" $ do
            pevalRotateRightTerm (ssymTerm "a" :: Term (WordN 4)) (conTerm 2)
              @?= rotateRightTerm (ssymTerm "a" :: Term (WordN 4)) (conTerm 2),
          testCase "Regression: rotate by very large number" $ do
            pevalRotateRightTerm (conTerm 15 :: Term (IntN 128)) (conTerm maxBound) @?= conTerm (rotateL 15 1)
            pevalRotateRightTerm (conTerm 15 :: Term (WordN 128)) (conTerm maxBound) @?= conTerm (rotateL 15 1)
        ]
    ]

concreteSmallRotateRightCorrect ::
  (PEvalRotateTerm a, Integral a, FiniteBits a, SupportedPrim a) =>
  a ->
  a ->
  Property
concreteSmallRotateRightCorrect _ b | b < 0 = discard
concreteSmallRotateRightCorrect a b = ioProperty $ do
  pevalRotateRightTerm (conTerm a) (conTerm b)
    @?= conTerm (rotateR a (fromIntegral b))
