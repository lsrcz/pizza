{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Backend.TermRewritingTests
  ( termRewritingTests,
    validateSpec,
    bitwuzlaConfig,
  )
where

import Data.Foldable (traverse_)
import Data.Proxy (Proxy (Proxy))
import GHC.TypeLits (KnownNat, type (<=))
import Grisette
  ( AlgReal,
    BitCast (bitCast),
    GrisetteSMTConfig,
    IEEEFPConstants
      ( fpMaxNormalized,
        fpMaxSubnormal,
        fpMinNormalized,
        fpMinSubnormal,
        fpNaN,
        fpNegativeInfinite,
        fpNegativeZero,
        fpPositiveInfinite,
        fpPositiveZero
      ),
    IEEEFPRoundingMode (rna, rne, rtn, rtp, rtz),
    ITEOp (symIte),
    IntN,
    LinkedRep,
    LogicalOp (symNot, true),
    Solvable (con),
    SymBool (SymBool),
    SymFP,
    SymIEEEFPTraits (symFpIsNaN),
    SymRep (SymType),
    WordN,
    bitwuzla,
    fpIsNaN,
    solve,
    z3,
  )
import Grisette.Backend.TermRewritingGen
  ( BoolOnlySpec (BoolOnlySpec),
    BoolWithFixedSizedBVSpec (BoolWithFixedSizedBVSpec),
    BoolWithLIASpec,
    DifferentSizeBVSpec,
    FPRoundingModeBoolOpSpec,
    FPRoundingModeSpec,
    FixedSizedBVWithBoolSpec (FixedSizedBVWithBoolSpec),
    GeneralSpec,
    IEEEFPBoolOpSpec (IEEEFPBoolOpSpec),
    IEEEFPSpec,
    LIAWithBoolSpec,
    NRAWithBoolSpec,
    TermRewritingSpec
      ( conSpec,
        counterExample,
        norewriteVer,
        rewriteVer,
        same,
        symSpec,
        wrap
      ),
    absNumSpec,
    addNumSpec,
    andBitsSpec,
    andSpec,
    bitCastOrSpec,
    bitCastSpec,
    bvconcatSpec,
    bvextendSpec,
    bvselectSpec,
    complementBitsSpec,
    divIntegralSpec,
    eqvSpec,
    fpBinaryOpSpec,
    fpFMASpec,
    fpRoundingBinarySpec,
    fpRoundingUnaryOpSpec,
    fpTraitSpec,
    fromFPOrSpec,
    iteSpec,
    leOrdSpec,
    modIntegralSpec,
    mulNumSpec,
    negNumSpec,
    notSpec,
    orBitsSpec,
    orSpec,
    quotIntegralSpec,
    remIntegralSpec,
    shiftLeftSpec,
    shiftRightSpec,
    signumNumSpec,
    toFPSpec,
    xorBitsSpec,
  )
import Grisette.Internal.Core.Data.Class.LogicalOp (LogicalOp ((.&&)))
import Grisette.Internal.Core.Data.Class.SymEq (SymEq ((./=), (.==)))
import Grisette.Internal.Core.Data.Class.SymIEEEFP
  ( SymIEEEFPTraits (symFpIsPositiveInfinite),
  )
import Grisette.Internal.SymPrim.FP
  ( ConvertibleBound (convertibleLowerBound, convertibleUpperBound),
    FP,
    FP32,
    FPRoundingMode,
    ValidFP,
    nextFP,
    prevFP,
  )
import Grisette.Internal.SymPrim.Prim.Term
  ( FPBinaryOp (FPMaximum, FPMaximumNumber, FPMinimum, FPMinimumNumber, FPRem),
    FPRoundingBinaryOp (FPAdd, FPDiv, FPMul, FPSub),
    FPRoundingUnaryOp (FPRoundToIntegral, FPSqrt),
    FPTrait (FPIsPositive),
    PEvalBVTerm,
    PEvalBitCastOrTerm,
    PEvalBitCastTerm,
    PEvalBitwiseTerm (pevalAndBitsTerm, pevalOrBitsTerm),
    PEvalIEEEFPConvertibleTerm,
    PEvalShiftTerm,
    SupportedNonFuncPrim,
    Term,
    andBitsTerm,
    andTerm,
    bvConcatTerm,
    bvExtendTerm,
    complementBitsTerm,
    conTerm,
    eqTerm,
    fpTraitTerm,
    iteTerm,
    notTerm,
    orBitsTerm,
    orTerm,
    pformatTerm,
    ssymTerm,
    xorBitsTerm,
  )
import Grisette.Internal.SymPrim.SymFP (SymFP32)
import Test.Framework (Test, TestName, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit (Assertion, assertFailure, (@?=))
import Test.QuickCheck
  ( Arbitrary,
    elements,
    forAll,
    ioProperty,
    mapSize,
    vectorOf,
    withMaxSuccess,
    (==>),
  )
import Type.Reflection (Typeable, typeRep)

validateSpec' ::
  (TermRewritingSpec a av) =>
  GrisetteSMTConfig ->
  SymBool ->
  a ->
  Assertion
validateSpec' config precond a = do
  r <- solve config (precond .&& SymBool (counterExample a))
  rs <- solve config (precond .&& SymBool (same a))
  case (r, rs) of
    (Left _, Right _) -> do
      return ()
    (Left _, Left err) -> do
      print err
      assertFailure $
        "Bad rewriting with unsolvable formula: "
          ++ pformatTerm (norewriteVer a)
          ++ " was rewritten to "
          ++ pformatTerm (rewriteVer a)
          ++ " under precondition"
          ++ show precond
          ++ " corresponding same formula:"
          ++ pformatTerm (same a)
    (Right m, _) -> do
      assertFailure $
        "With model"
          ++ show m
          ++ "Bad rewriting: "
          ++ pformatTerm (norewriteVer a)
          ++ " was rewritten to "
          ++ pformatTerm (rewriteVer a)
          ++ " under precondition"
          ++ show precond
          ++ " corresponding cex formula:"
          ++ pformatTerm (counterExample a)
          ++ "\n"
          ++ show (norewriteVer a)
          ++ "\n"
          ++ show (rewriteVer a)
          ++ "\n"
          ++ show (counterExample a)

validateSpec ::
  (TermRewritingSpec a av) =>
  GrisetteSMTConfig ->
  a ->
  Assertion
validateSpec config = validateSpec' config true

bitwuzlaConfig :: IO (Maybe GrisetteSMTConfig)
bitwuzlaConfig = do
  v <-
    solve bitwuzla $
      (("x" :: SymFP32) ./= "x")
        .&& symNot (symFpIsPositiveInfinite (con $ -4.7e-38 :: SymFP32))
        .&& ( symIte
                "bool"
                (con fpPositiveInfinite :: SymFP32)
                (con fpNegativeInfinite)
                .== "m"
            )
  case v of
    Left _ -> return Nothing
    Right _ -> return $ Just bitwuzla

onlyWhenBitwuzlaIsAvailable :: (GrisetteSMTConfig -> IO ()) -> IO ()
onlyWhenBitwuzlaIsAvailable action = do
  config <- bitwuzlaConfig
  case config of
    Just config -> action config
    Nothing ->
      putStrLn $
        "bitwuzla isn't available in the system, or the dependent sbv"
          <> " library does not work well with it. This test is marked as "
          <> " success."

unboundedConfig = z3

divisionTest ::
  forall a b.
  (TermRewritingSpec a b, Enum b, Num b) =>
  TestName ->
  (a -> a -> a) ->
  Test
divisionTest name f =
  testGroup
    name
    [ testCase "on concrete" $ do
        traverse_
          ( \(x :: b, y :: b) -> do
              validateSpec @a unboundedConfig $ f (conSpec x) (conSpec y)
          )
          [(i, j) | i <- [-3 .. 3], j <- [-3 .. 3]],
      testCase "on single concrete" $ do
        traverse_
          ( \x -> do
              validateSpec @a unboundedConfig $ f (conSpec x) (symSpec "a")
              validateSpec @a unboundedConfig $ f (symSpec "a") (conSpec x)
          )
          [-3 .. 3]
    ]

bvConcatTest ::
  forall bv.
  ( forall n. (KnownNat n, 1 <= n) => SupportedNonFuncPrim (bv n),
    forall n. (KnownNat n, 1 <= n) => PEvalShiftTerm (bv n),
    forall n. (KnownNat n, 1 <= n) => Arbitrary (bv n),
    Typeable bv,
    PEvalBVTerm bv
  ) =>
  Test
bvConcatTest =
  testGroup (show (typeRep @bv) <> "_concat") $
    ( do
        (opName, opSpec, termOp) <-
          [ ("and", andBitsSpec, andBitsTerm),
            ("or", orBitsSpec, orBitsTerm),
            ("xor", xorBitsSpec, xorBitsTerm)
          ]
        [ testCase (opName <> "(const,concat)") $ do
            let lhs = conSpec 0x39 :: FixedSizedBVWithBoolSpec bv 8
            let rhs =
                  bvconcatSpec
                    (symSpec "a" :: FixedSizedBVWithBoolSpec bv 4)
                    (symSpec "b" :: FixedSizedBVWithBoolSpec bv 4)
            let spec = opSpec lhs rhs
            validateSpec @(FixedSizedBVWithBoolSpec bv 8) unboundedConfig spec,
          testCase (opName <> "(concat,concat)") $ do
            let lhs =
                  bvconcatSpec
                    (symSpec "a" :: FixedSizedBVWithBoolSpec bv 4)
                    (symSpec "b" :: FixedSizedBVWithBoolSpec bv 4)
            let rhs =
                  bvconcatSpec
                    (symSpec "c" :: FixedSizedBVWithBoolSpec bv 4)
                    (symSpec "d" :: FixedSizedBVWithBoolSpec bv 4)
            let spec@(FixedSizedBVWithBoolSpec _ r) = opSpec lhs rhs
            let expected =
                  ( bvConcatTerm
                      (termOp (ssymTerm "a" :: Term (bv 4)) (ssymTerm "c"))
                      (termOp (ssymTerm "b" :: Term (bv 4)) (ssymTerm "d"))
                  )
            r @?= expected
            validateSpec @(FixedSizedBVWithBoolSpec bv 8) unboundedConfig spec
          ]
    )
      ++ [ testCase "complement(concat)" $ do
             let lhs =
                   bvconcatSpec
                     (symSpec "a" :: FixedSizedBVWithBoolSpec bv 4)
                     (symSpec "b" :: FixedSizedBVWithBoolSpec bv 4)
             let spec@(FixedSizedBVWithBoolSpec _ r) = complementBitsSpec lhs
             let expected =
                   bvConcatTerm
                     (complementBitsTerm (ssymTerm "a" :: Term (bv 4)))
                     (complementBitsTerm (ssymTerm "b" :: Term (bv 4)))
             r @?= expected
             validateSpec @(FixedSizedBVWithBoolSpec bv 8) unboundedConfig spec,
           testCase "complement(sext)" $ do
             let spec@(FixedSizedBVWithBoolSpec _ r) =
                   complementBitsSpec
                     ( bvextendSpec
                         True
                         (Proxy :: Proxy 8)
                         (symSpec "a" :: FixedSizedBVWithBoolSpec bv 4)
                     ) ::
                     FixedSizedBVWithBoolSpec bv 8
             let expected =
                   bvExtendTerm
                     True
                     (Proxy :: Proxy 8)
                     (complementBitsTerm (ssymTerm "a" :: Term (bv 4)))
             r @?= expected
             validateSpec @(FixedSizedBVWithBoolSpec bv 8) unboundedConfig spec,
           testCase "and(leading_zero_then_all_one, b)" $ do
             let spec =
                   andBitsSpec
                     (conSpec 0x1f)
                     (symSpec "b" :: FixedSizedBVWithBoolSpec bv 4)
             validateSpec @(FixedSizedBVWithBoolSpec bv 4) unboundedConfig spec,
           testCase "and(leading_one_then_all_zero, b)" $ do
             let spec =
                   andBitsSpec
                     (conSpec 0xf8)
                     (symSpec "b" :: FixedSizedBVWithBoolSpec bv 4)
             validateSpec @(FixedSizedBVWithBoolSpec bv 4) unboundedConfig spec,
           testCase "or(leading_zero_then_all_one, b)" $ do
             let spec =
                   orBitsSpec
                     (conSpec 0x1f)
                     (symSpec "b" :: FixedSizedBVWithBoolSpec bv 4)
             validateSpec @(FixedSizedBVWithBoolSpec bv 4) unboundedConfig spec,
           testCase "or(leading_one_then_all_zero, b)" $ do
             let spec =
                   orBitsSpec
                     (conSpec 0xf8)
                     (symSpec "b" :: FixedSizedBVWithBoolSpec bv 4)
             validateSpec @(FixedSizedBVWithBoolSpec bv 4) unboundedConfig spec,
           testCase "select(sext)with_part_base" $ do
             let spec =
                   bvselectSpec
                     (Proxy @2)
                     (Proxy @4)
                     ( bvextendSpec
                         True
                         (Proxy @8)
                         (symSpec "b" :: FixedSizedBVWithBoolSpec bv 4) ::
                         FixedSizedBVWithBoolSpec bv 8
                     ) ::
                     FixedSizedBVWithBoolSpec bv 4
             validateSpec @(FixedSizedBVWithBoolSpec bv 4) unboundedConfig spec,
           testCase "ite(cond,concat(a,b),concat(c,d))" $ do
             let spec@(FixedSizedBVWithBoolSpec _ r) =
                   iteSpec
                     (symSpec "cond" :: BoolWithFixedSizedBVSpec bv 4)
                     ( bvconcatSpec
                         (symSpec "a" :: FixedSizedBVWithBoolSpec bv 4)
                         (symSpec "b" :: FixedSizedBVWithBoolSpec bv 4)
                     )
                     ( bvconcatSpec
                         (symSpec "c" :: FixedSizedBVWithBoolSpec bv 4)
                         (symSpec "d" :: FixedSizedBVWithBoolSpec bv 4)
                     )
             let expected =
                   bvConcatTerm
                     ( iteTerm
                         (ssymTerm "cond" :: Term Bool)
                         (ssymTerm "a" :: Term (bv 4))
                         (ssymTerm "c" :: Term (bv 4))
                     )
                     ( iteTerm
                         (ssymTerm "cond" :: Term Bool)
                         (ssymTerm "b" :: Term (bv 4))
                         (ssymTerm "d" :: Term (bv 4))
                     )
             r @?= expected
             validateSpec @(FixedSizedBVWithBoolSpec bv 8) unboundedConfig spec
         ]
      ++ ( do
             pos <- [True, False]
             cvalue <- [0 :: Int, -1]
             value <- [0 :: Int, -1]
             let con = conSpec (fromIntegral value) :: FixedSizedBVWithBoolSpec bv 4
             let sym = symSpec "b"
             let t = if pos then con else sym
             let f = if pos then sym else con
             let spec =
                   iteSpec
                     ( eqvSpec
                         (symSpec "c" :: FixedSizedBVWithBoolSpec bv 1)
                         (conSpec $ fromIntegral cvalue) ::
                         BoolWithFixedSizedBVSpec bv 1
                     )
                     t
                     f
             let ts = if pos then show value else "sym"
             let fs = if pos then "sym" else show value
             return
               $ testCase
                 ( "ite(bv1="
                     <> show cvalue
                     <> ","
                     <> ts
                     <> ","
                     <> fs
                     <> ")"
                 )
               $ validateSpec unboundedConfig spec
         )
      ++ ( do
             let cond = symSpec "cond" :: BoolWithFixedSizedBVSpec bv 1
             let condTerm = ssymTerm "cond" :: Term Bool
             (opName, op, opSpec) <-
               [ ("and", andBitsTerm, andBitsSpec),
                 ("or", orBitsTerm, orBitsSpec),
                 ("xor", xorBitsTerm, xorBitsSpec),
                 ("ite", iteTerm condTerm, iteSpec cond)
               ]
             let name = opName <> "(sext,sext)"
             let spec@(FixedSizedBVWithBoolSpec _ r) =
                   opSpec
                     ( bvextendSpec
                         True
                         (Proxy :: Proxy 8)
                         (symSpec "a" :: FixedSizedBVWithBoolSpec bv 4)
                     )
                     ( bvextendSpec
                         True
                         (Proxy :: Proxy 8)
                         (symSpec "b" :: FixedSizedBVWithBoolSpec bv 4)
                     )
             let expected =
                   bvExtendTerm
                     True
                     (Proxy :: Proxy 8)
                     ( op
                         (ssymTerm "a" :: Term (bv 4))
                         (ssymTerm "b" :: Term (bv 4))
                     )
             return $ testCase name $ do
               r @?= expected
               validateSpec @(FixedSizedBVWithBoolSpec bv 8) unboundedConfig spec
         )
      ++ ( do
             let a = symSpec "a" :: FixedSizedBVWithBoolSpec bv 4
             let b = symSpec "b" :: FixedSizedBVWithBoolSpec bv 4
             let c = symSpec "c" :: FixedSizedBVWithBoolSpec bv 4
             let cond = symSpec "cond" :: BoolWithFixedSizedBVSpec bv 1
             (lhs, rhs, lhsName, rhsName) <-
               [ (andBitsSpec a b, a, "(& a b)", "a"),
                 (andBitsSpec a b, b, "(& a b)", "b"),
                 (a, andBitsSpec a b, "a", "(& a b)"),
                 (b, andBitsSpec a b, "b", "(& a b)"),
                 (orBitsSpec a b, a, "(| a b)", "a"),
                 (orBitsSpec a b, b, "(| a b)", "b"),
                 (a, orBitsSpec a b, "a", "(| a b)"),
                 (b, orBitsSpec a b, "b", "(| a b)"),
                 (andBitsSpec a b, andBitsSpec a c, "(& a b)", "(& a c)"),
                 (andBitsSpec a b, andBitsSpec c a, "(& a b)", "(& c a)"),
                 (andBitsSpec a b, andBitsSpec b c, "(& a b)", "(& b c)"),
                 (andBitsSpec a b, andBitsSpec c b, "(& a b)", "(& c b)"),
                 (orBitsSpec a b, orBitsSpec a c, "(| a b)", "(| a c)"),
                 (orBitsSpec a b, orBitsSpec c a, "(| a b)", "(| c a)"),
                 (orBitsSpec a b, orBitsSpec b c, "(| a b)", "(| b c)"),
                 (orBitsSpec a b, orBitsSpec c b, "(| a b)", "(| c b)")
               ]
             let spec = iteSpec cond lhs rhs
             return $
               testCase ("ite(cond," <> lhsName <> "," <> rhsName <> ")") $
                 validateSpec @(FixedSizedBVWithBoolSpec bv 4) unboundedConfig spec
         )
      ++ ( do
             trueBranch <- [True, False]
             (opName, spec) <-
               [("and", andBitsSpec), ("or", orBitsSpec)]
             let iteName = "ite(cond,const,const)"
             let name =
                   opName
                     <> "("
                     <> (if trueBranch then iteName else "a")
                     <> ","
                     <> (if trueBranch then "b" else iteName)
                     <> ")"
             return $ testProperty name $ \a b -> ioProperty $ do
               let ite =
                     iteSpec
                       (symSpec "cond" :: BoolWithFixedSizedBVSpec bv 4)
                       (conSpec a :: FixedSizedBVWithBoolSpec bv 4)
                       (conSpec b :: FixedSizedBVWithBoolSpec bv 4)
               let s = symSpec "a" :: FixedSizedBVWithBoolSpec bv 4
               let lhs = if trueBranch then ite else s
               let rhs = if trueBranch then s else ite
               let resSpec = spec lhs rhs
               validateSpec unboundedConfig resSpec
         )
      ++ ( do
             a <- [0, 1, 4, 7, 8, 9, 16]
             (opName, opSpec) <-
               [ ("shr", shiftRightSpec),
                 ("shl", shiftLeftSpec)
               ]
             return $ testCase (opName <> "(bv8," <> show a <> ")") $ do
               let spec =
                     opSpec
                       (symSpec "a" :: FixedSizedBVWithBoolSpec bv 8)
                       (conSpec a)
               validateSpec @(FixedSizedBVWithBoolSpec bv 8) unboundedConfig spec
         )
      ++ ( do
             trueBranch <- [True, False]
             innerTrueBranch <- [True, False]
             let l = symSpec "l" :: FixedSizedBVWithBoolSpec bv 8
             let r = symSpec "r" :: FixedSizedBVWithBoolSpec bv 8
             let cond = symSpec "cond" :: BoolWithFixedSizedBVSpec bv 8
             (absorbing, absorbingTerm, absorbingValueName, opName, opSpec, opTerm) <-
               [ ( conSpec 0 :: FixedSizedBVWithBoolSpec bv 8,
                   conTerm 0 :: Term (bv 8),
                   "0",
                   "and",
                   andBitsSpec,
                   andBitsTerm
                 ),
                 ( conSpec $ -1 :: FixedSizedBVWithBoolSpec bv 8,
                   conTerm $ -1 :: Term (bv 8),
                   "-1",
                   "or",
                   orBitsSpec,
                   orBitsTerm
                 )
               ]
             let condTerm = ssymTerm "cond" :: Term Bool
             let lTerm = ssymTerm "l" :: Term (bv 8)
             let rTerm = ssymTerm "r" :: Term (bv 8)

             let (lhsName, lhs, lhsResultTerm)
                   | trueBranch && innerTrueBranch = ("ite(cond," <> absorbingValueName <> ",l)", iteSpec cond absorbing l, absorbingTerm)
                   | trueBranch && not innerTrueBranch = ("ite(cond,l," <> absorbingValueName <> ")", iteSpec cond l absorbing, opTerm lTerm rTerm)
                   | otherwise = ("l", l, if innerTrueBranch then absorbingTerm else opTerm lTerm rTerm)
             let (rhsName, rhs, rhsResultTerm)
                   | trueBranch = ("r", r, if innerTrueBranch then opTerm lTerm rTerm else absorbingTerm)
                   | innerTrueBranch = ("ite(cond," <> absorbingValueName <> ",r)", iteSpec cond absorbing r, opTerm lTerm rTerm)
                   | otherwise = ("ite(cond,r," <> absorbingValueName <> ")", iteSpec cond r absorbing, absorbingTerm)
             let spec@(FixedSizedBVWithBoolSpec _ rspec) = opSpec lhs rhs
             return $ testCase (opName <> "(" <> lhsName <> "," <> rhsName <> ")") $ do
               rspec @?= iteTerm condTerm lhsResultTerm rhsResultTerm
               validateSpec @(FixedSizedBVWithBoolSpec bv 8) unboundedConfig spec
         )
      ++ ( do
             innerTrueBranch <- [0 :: Int, -1]
             let innerFalseBranch = -1 - innerTrueBranch
             lhsIsExtend <- [True, False]
             let extendName =
                   "sext(ite(cond,"
                     <> show innerTrueBranch
                     <> ","
                     <> show innerFalseBranch
                     <> "))"
             (opName, opSpec, opTerm) <-
               [ ("and", andBitsSpec, pevalAndBitsTerm),
                 ("or", orBitsSpec, pevalOrBitsTerm)
               ]
             let name =
                   opName
                     <> if lhsIsExtend then "(" <> extendName <> ",c)" else "(c," <> extendName <> ")"
             return $
               testCase name $
                 do
                   let extend =
                         bvextendSpec
                           True
                           (Proxy :: Proxy 8)
                           ( iteSpec
                               (symSpec "cond" :: BoolWithFixedSizedBVSpec bv 1)
                               (conSpec $ fromIntegral innerTrueBranch :: FixedSizedBVWithBoolSpec bv 4)
                               (conSpec $ fromIntegral innerFalseBranch :: FixedSizedBVWithBoolSpec bv 4)
                           ) ::
                           FixedSizedBVWithBoolSpec bv 8
                   let single = symSpec "c" :: FixedSizedBVWithBoolSpec bv 8
                   let spec@(FixedSizedBVWithBoolSpec _ r) =
                         if lhsIsExtend
                           then opSpec extend single
                           else opSpec single extend
                   let expected =
                         iteTerm
                           (ssymTerm "cond" :: Term Bool)
                           (opTerm (conTerm $ fromIntegral innerTrueBranch) (ssymTerm "c"))
                           (opTerm (conTerm $ fromIntegral innerFalseBranch) (ssymTerm "c"))
                   r @?= expected
                   validateSpec @(FixedSizedBVWithBoolSpec bv 8) unboundedConfig spec
         )

bv1Test ::
  forall bv.
  ( SupportedNonFuncPrim (bv 1),
    Num (bv 1),
    Typeable bv,
    PEvalBitwiseTerm (bv 1)
  ) =>
  Test
bv1Test =
  testGroup (show (typeRep @bv) <> " 1") $
    ( do
        (opName, op, v, bvop) <-
          [ ("||", orSpec, 0, andBitsTerm),
            ("||", orSpec, 1, orBitsTerm),
            ("&&", andSpec, 0, orBitsTerm),
            ("&&", andSpec, 1, andBitsTerm)
          ]
        let isV = "==" <> show v
        let name = "(a" <> isV <> ")" <> opName <> "(b" <> isV <> ")"
        let spec@(BoolWithFixedSizedBVSpec _ r) =
              op
                (eqvSpec (symSpec "a" :: FixedSizedBVWithBoolSpec bv 1) (conSpec v))
                (eqvSpec (symSpec "b" :: FixedSizedBVWithBoolSpec bv 1) (conSpec v))
        let expected =
              eqTerm
                ( bvop
                    (ssymTerm "a" :: Term (bv 1))
                    (ssymTerm "b")
                )
                (conTerm v)
        return $ testCase name $ do
          r @?= expected
          validateSpec @(BoolWithFixedSizedBVSpec bv 1) unboundedConfig spec
    )
      ++ ( do
             cond <- [0 :: bv 1, 1]
             t <- [0 :: bv 1, 1]
             let f = 1 - t
             let name = "ite(a==" <> show cond <> "," <> show t <> "," <> show f <> ")"
             let spec@(FixedSizedBVWithBoolSpec _ r) =
                   iteSpec
                     ( eqvSpec
                         (symSpec "a" :: FixedSizedBVWithBoolSpec bv 1)
                         (conSpec cond) ::
                         BoolWithFixedSizedBVSpec bv 1
                     )
                     (conSpec t)
                     (conSpec f)
             let aTerm = ssymTerm "a" :: Term (bv 1)
             let expected =
                   if t == cond
                     then aTerm
                     else complementBitsTerm aTerm
             return $ testCase name $ do
               r @?= expected
               validateSpec @(FixedSizedBVWithBoolSpec bv 1) unboundedConfig spec
         )
      ++ [ testCase "(a==b)==(a==c)" $ do
             let spec@(BoolWithFixedSizedBVSpec _ r) =
                   eqvSpec
                     (eqvSpec (symSpec "a" :: FixedSizedBVWithBoolSpec bv 1) (symSpec "b") :: BoolWithFixedSizedBVSpec bv 1)
                     (eqvSpec (symSpec "a" :: FixedSizedBVWithBoolSpec bv 1) (symSpec "c"))
             validateSpec @(BoolWithFixedSizedBVSpec bv 1)
               unboundedConfig
               spec
             r @?= eqTerm (ssymTerm "b" :: Term (bv 1)) (ssymTerm "c")
         ]

termRewritingTests :: Test
termRewritingTests =
  testGroup
    "TermRewriting"
    [ bv1Test @WordN,
      bv1Test @IntN,
      bvConcatTest @WordN,
      bvConcatTest @IntN,
      testGroup
        "Bool only"
        [ testProperty "Bool only random test" $
            mapSize (`min` 10) $
              ioProperty . \(x :: BoolOnlySpec) -> do
                validateSpec unboundedConfig x,
          testCase "Regression nested ite with (ite a (ite b c d) e) with b is true" $ do
            validateSpec @BoolOnlySpec
              unboundedConfig
              ( iteSpec
                  (symSpec "a" :: BoolOnlySpec)
                  ( iteSpec
                      (orSpec (notSpec (andSpec (symSpec "b1") (symSpec "b2"))) (symSpec "b2") :: BoolOnlySpec)
                      (symSpec "c")
                      (symSpec "d")
                  )
                  (symSpec "e")
              ),
          testCase "Regression for pevalImpliesTerm _ false should be false" $ do
            validateSpec @BoolOnlySpec
              unboundedConfig
              ( iteSpec
                  (symSpec "fbool" :: BoolOnlySpec)
                  ( notSpec
                      ( orSpec
                          (orSpec (notSpec (andSpec (symSpec "gbool" :: BoolOnlySpec) (symSpec "fbool" :: BoolOnlySpec))) (symSpec "gbool" :: BoolOnlySpec))
                          (orSpec (symSpec "abool" :: BoolOnlySpec) (notSpec (andSpec (symSpec "gbool" :: BoolOnlySpec) (symSpec "bbool" :: BoolOnlySpec))))
                      )
                  )
                  (symSpec "xxx" :: BoolOnlySpec)
              ),
          testCase "And absortion" $ do
            let spec@(BoolOnlySpec _ r) =
                  andSpec
                    (orSpec (symSpec "c") (orSpec (symSpec "b") (symSpec "a")))
                    (andSpec (symSpec "a") (andSpec (symSpec "d") (symSpec "e")))
            validateSpec @BoolOnlySpec
              unboundedConfig
              spec
            r @?= andTerm (ssymTerm "a") (andTerm (ssymTerm "d") (ssymTerm "e")),
          testCase "Or absorption" $ do
            let spec@(BoolOnlySpec _ r) =
                  orSpec
                    (andSpec (symSpec "c") (andSpec (symSpec "b") (symSpec "a")))
                    (orSpec (symSpec "a") (orSpec (symSpec "d") (symSpec "e")))
            validateSpec @BoolOnlySpec
              unboundedConfig
              spec
            r @?= orTerm (ssymTerm "a") (orTerm (ssymTerm "d") (ssymTerm "e")),
          testCase "(a==b)==(a==c)" $ do
            let spec@(BoolOnlySpec _ r) =
                  eqvSpec
                    (eqvSpec (symSpec "a" :: BoolOnlySpec) (symSpec "b") :: BoolOnlySpec)
                    (eqvSpec (symSpec "a" :: BoolOnlySpec) (symSpec "c"))
            validateSpec @BoolOnlySpec
              unboundedConfig
              spec
            r @?= eqTerm (ssymTerm "b" :: Term Bool) (ssymTerm "c")
        ],
      testGroup
        "LIA"
        [ testProperty "LIA random test" $
            mapSize (`min` 10) $
              ioProperty . \(x :: LIAWithBoolSpec) -> do
                validateSpec unboundedConfig x,
          testCase "Regression nested ite with (ite a b (ite c d e)) with c implies a" $ do
            validateSpec @LIAWithBoolSpec
              unboundedConfig
              ( iteSpec
                  (notSpec (eqvSpec (symSpec "v" :: LIAWithBoolSpec) (conSpec 1 :: LIAWithBoolSpec) :: BoolWithLIASpec))
                  (symSpec "b")
                  ( iteSpec
                      (eqvSpec (symSpec "v" :: LIAWithBoolSpec) (conSpec 2 :: LIAWithBoolSpec) :: BoolWithLIASpec)
                      (symSpec "d")
                      (symSpec "d")
                  )
              )
        ],
      testGroup
        "NRA"
        [ testProperty "NRA random test" $
            mapSize (`min` 5) $
              ioProperty . \(x :: NRAWithBoolSpec) ->
                validateSpec unboundedConfig x
        ],
      testGroup
        "Different sized signed BV"
        [ testProperty "Random test" $
            withMaxSuccess 1000 . mapSize (`min` 5) $
              ioProperty . \(x :: (DifferentSizeBVSpec IntN 4)) -> do
                validateSpec unboundedConfig x
        ],
      testGroup
        "Fixed sized signed BV"
        [ testProperty "Random test on IntN 1" $
            withMaxSuccess 200 . mapSize (`min` 5) $
              ioProperty . \(x :: (FixedSizedBVWithBoolSpec IntN 1)) -> do
                validateSpec unboundedConfig x,
          testProperty "Random test on IntN 2" $
            withMaxSuccess 200 . mapSize (`min` 5) $
              ioProperty . \(x :: (FixedSizedBVWithBoolSpec IntN 2)) -> do
                validateSpec unboundedConfig x,
          testProperty "Random test on IntN 4" $
            withMaxSuccess 200 . mapSize (`min` 5) $
              ioProperty . \(x :: (FixedSizedBVWithBoolSpec IntN 4)) -> do
                validateSpec unboundedConfig x,
          testProperty "Random test on IntN 63" $
            withMaxSuccess 200 . mapSize (`min` 1) $
              ioProperty . \(x :: (FixedSizedBVWithBoolSpec IntN 63)) -> do
                validateSpec unboundedConfig x,
          testProperty "Random test on IntN 64" $
            withMaxSuccess 200 . mapSize (`min` 1) $
              ioProperty . \(x :: (FixedSizedBVWithBoolSpec IntN 64)) -> do
                validateSpec unboundedConfig x,
          testProperty "Random test on IntN 65" $
            withMaxSuccess 200 . mapSize (`min` 1) $
              ioProperty . \(x :: (FixedSizedBVWithBoolSpec IntN 65)) -> do
                validateSpec unboundedConfig x,
          testProperty "Random test on IntN 128" $
            withMaxSuccess 200 . mapSize (`min` 1) $
              ioProperty . \(x :: (FixedSizedBVWithBoolSpec IntN 128)) -> do
                validateSpec unboundedConfig x
        ],
      testGroup
        "Different sized unsigned BV"
        [ testProperty "random test" $
            withMaxSuccess 1000 . mapSize (`min` 5) $
              ioProperty . \(x :: (DifferentSizeBVSpec WordN 4)) -> do
                validateSpec unboundedConfig x
        ],
      testGroup
        "Fixed sized unsigned BV"
        [ testProperty "Random test on WordN 1" $
            withMaxSuccess 200 . mapSize (`min` 5) $
              ioProperty . \(x :: (FixedSizedBVWithBoolSpec WordN 1)) -> do
                validateSpec unboundedConfig x,
          testProperty "Random test on WordN 2" $
            withMaxSuccess 200 . mapSize (`min` 5) $
              ioProperty . \(x :: (FixedSizedBVWithBoolSpec WordN 2)) -> do
                validateSpec unboundedConfig x,
          testProperty "Random test on WordN 4" $
            withMaxSuccess 200 . mapSize (`min` 5) $
              ioProperty . \(x :: (FixedSizedBVWithBoolSpec WordN 4)) -> do
                validateSpec unboundedConfig x,
          testProperty "Random test on WordN 63" $
            withMaxSuccess 200 . mapSize (`min` 1) $
              ioProperty . \(x :: (FixedSizedBVWithBoolSpec WordN 63)) -> do
                validateSpec unboundedConfig x,
          testProperty "Random test on WordN 64" $
            withMaxSuccess 200 . mapSize (`min` 1) $
              ioProperty . \(x :: (FixedSizedBVWithBoolSpec WordN 64)) -> do
                validateSpec unboundedConfig x,
          testProperty "Random test on WordN 65" $
            withMaxSuccess 200 . mapSize (`min` 1) $
              ioProperty . \(x :: (FixedSizedBVWithBoolSpec WordN 65)) -> do
                validateSpec unboundedConfig x,
          testProperty "Random test on WordN 128" $
            withMaxSuccess 200 . mapSize (`min` 1) $
              ioProperty . \(x :: (FixedSizedBVWithBoolSpec WordN 128)) -> do
                validateSpec unboundedConfig x
        ],
      testCase "Regression: shift twice and the sum of shift amount overflows" $ do
        validateSpec @(FixedSizedBVWithBoolSpec IntN 4)
          unboundedConfig
          ( shiftRightSpec
              (shiftRightSpec (symSpec "fint") (conSpec 0x5))
              (conSpec 0x5)
          ),
      testGroup
        "Regression for abs on unsigned BV"
        [ testCase "abs on negate" $
            validateSpec @(FixedSizedBVWithBoolSpec WordN 4)
              unboundedConfig
              (absNumSpec (negNumSpec (symSpec "a"))),
          testCase "abs on times negate" $
            validateSpec @(FixedSizedBVWithBoolSpec WordN 4)
              unboundedConfig
              (absNumSpec (mulNumSpec (symSpec "a") (negNumSpec (symSpec "b"))))
        ],
      testGroup
        "mulNumSpec on integer"
        [ testCase "times on both concrete" $ do
            traverse_
              (\(x, y) -> validateSpec @(GeneralSpec Integer) unboundedConfig $ mulNumSpec (conSpec x) (conSpec y))
              [(i, j) | i <- [-3 .. 3], j <- [-3 .. 3]],
          testCase "times on single concrete" $ do
            traverse_
              ( \x -> do
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ mulNumSpec (conSpec x) (symSpec "a")
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ mulNumSpec (symSpec "a") (conSpec x)
              )
              [-3 .. 3],
          testCase "Two times with two concrete combined" $ do
            traverse_
              ( \(x, y) -> do
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ mulNumSpec (conSpec x) $ mulNumSpec (conSpec y) (symSpec "a")
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ mulNumSpec (conSpec x) $ mulNumSpec (symSpec "a") (conSpec y)
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ mulNumSpec (mulNumSpec (conSpec x) (symSpec "a")) (conSpec y)
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ mulNumSpec (mulNumSpec (symSpec "a") (conSpec x)) (conSpec y)
              )
              [(i, j) | i <- [-3 .. 3], j <- [-3 .. 3]],
          testCase "Two times with one concrete" $ do
            traverse_
              ( \x -> do
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ mulNumSpec (conSpec x) $ mulNumSpec (symSpec "b") (symSpec "a")
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ mulNumSpec (symSpec "b") $ mulNumSpec (symSpec "a") (conSpec x)
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ mulNumSpec (symSpec "b") $ mulNumSpec (conSpec x) (symSpec "a")
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ mulNumSpec (mulNumSpec (conSpec x) (symSpec "a")) (symSpec "b")
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ mulNumSpec (mulNumSpec (symSpec "a") (conSpec x)) (symSpec "b")
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ mulNumSpec (mulNumSpec (symSpec "a") (symSpec "b")) (conSpec x)
              )
              [-3 .. 3],
          testCase "times and add with two concretes combined" $ do
            traverse_
              ( \(x, y) -> do
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ mulNumSpec (conSpec x) $ addNumSpec (conSpec y) (symSpec "a")
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ mulNumSpec (conSpec x) $ addNumSpec (symSpec "a") (conSpec y)
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ mulNumSpec (addNumSpec (conSpec x) (symSpec "a")) (conSpec y)
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ mulNumSpec (addNumSpec (symSpec "a") (conSpec x)) (conSpec y)
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ addNumSpec (conSpec x) $ mulNumSpec (conSpec y) (symSpec "a")
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ addNumSpec (conSpec x) $ mulNumSpec (symSpec "a") (conSpec y)
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ addNumSpec (mulNumSpec (conSpec x) (symSpec "a")) (conSpec y)
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ addNumSpec (mulNumSpec (symSpec "a") (conSpec x)) (conSpec y)
              )
              [(i, j) | i <- [-3 .. 3], j <- [-3 .. 3]],
          testCase "times concrete with negNumSpec symbolic" $ do
            traverse_
              ( \x -> do
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ mulNumSpec (conSpec x) (negNumSpec $ symSpec "a")
                  validateSpec @(GeneralSpec Integer) unboundedConfig $ mulNumSpec (negNumSpec $ symSpec "a") (conSpec x)
              )
              [-3 .. 3]
        ],
      testGroup
        "divisions on integer"
        [ divisionTest @(GeneralSpec Integer) "div" divIntegralSpec,
          divisionTest @(GeneralSpec Integer) "mod" modIntegralSpec,
          divisionTest @(GeneralSpec Integer) "quot" quotIntegralSpec,
          divisionTest @(GeneralSpec Integer) "rem" remIntegralSpec
        ],
      testGroup
        "divisions on signed bv"
        [ divisionTest @(GeneralSpec (IntN 4)) "div" divIntegralSpec,
          divisionTest @(GeneralSpec (IntN 4)) "mod" modIntegralSpec,
          divisionTest @(GeneralSpec (IntN 4)) "quot" quotIntegralSpec,
          divisionTest @(GeneralSpec (IntN 4)) "rem" remIntegralSpec
        ],
      testGroup
        "divisions on unsigned bv"
        [ divisionTest @(GeneralSpec (WordN 4)) "div" divIntegralSpec,
          divisionTest @(GeneralSpec (WordN 4)) "mod" modIntegralSpec,
          divisionTest @(GeneralSpec (WordN 4)) "quot" quotIntegralSpec,
          divisionTest @(GeneralSpec (WordN 4)) "rem" remIntegralSpec
        ],
      testGroup
        "FP"
        [ testCase "0.0 == -0.0" $
            onlyWhenBitwuzlaIsAvailable
              ( `validateSpec`
                  ( eqvSpec
                      (conSpec 0.0 :: IEEEFPSpec 5 11)
                      (conSpec $ -0.0) ::
                      IEEEFPBoolOpSpec 5 11
                  )
              ),
          testCase "-0.0 <= -0.0" $
            onlyWhenBitwuzlaIsAvailable
              ( `validateSpec`
                  ( leOrdSpec
                      (conSpec 0.0 :: IEEEFPSpec 5 11)
                      (conSpec $ -0.0) ::
                      IEEEFPBoolOpSpec 5 11
                  )
              ),
          testCase "is_pos(nan)" $
            onlyWhenBitwuzlaIsAvailable
              ( `validateSpec`
                  ( fpTraitSpec
                      FPIsPositive
                      (conSpec fpNaN :: IEEEFPSpec 5 11) ::
                      IEEEFPBoolOpSpec 5 11
                  )
              ),
          testCase "is_pos(+inf)" $
            onlyWhenBitwuzlaIsAvailable
              ( `validateSpec`
                  ( fpTraitSpec
                      FPIsPositive
                      ( iteSpec
                          (symSpec "bool" :: BoolOnlySpec)
                          (conSpec fpNegativeInfinite)
                          (conSpec fpPositiveInfinite) ::
                          IEEEFPSpec 5 11
                      ) ::
                      IEEEFPBoolOpSpec 5 11
                  )
              ),
          testCase "regression 2" $
            onlyWhenBitwuzlaIsAvailable
              ( `validateSpec`
                  ( eqvSpec
                      (signumNumSpec (conSpec (1.175e-38) :: IEEEFPSpec 5 11))
                      (symSpec "b") ::
                      IEEEFPBoolOpSpec 5 11
                  )
              ),
          testCase "test sbv bug mitigation sbv#702" $
            onlyWhenBitwuzlaIsAvailable
              ( flip validateSpec $
                  IEEEFPBoolOpSpec
                    ( fpTraitTerm
                        FPIsPositive
                        ( iteTerm
                            (ssymTerm "bool")
                            (conTerm fpNegativeInfinite :: Term FP32)
                            (conTerm fpPositiveInfinite :: Term FP32)
                        )
                    )
                    (notTerm $ ssymTerm "bool")
              ),
          testProperty "FP32BoolOp" $
            withMaxSuccess 1000 . mapSize (`min` 10) $
              ioProperty . \(x :: IEEEFPBoolOpSpec 5 11) ->
                onlyWhenBitwuzlaIsAvailable (`validateSpec` x),
          testProperty "FPRoundingModeBoolOpSpec" $
            mapSize (`min` 10) $
              ioProperty . \(x :: FPRoundingModeBoolOpSpec) ->
                onlyWhenBitwuzlaIsAvailable (`validateSpec` x),
          testGroup "fpBinaryOp" $ do
            op <-
              [FPMaximum, FPMinimum, FPMaximumNumber, FPMinimumNumber, FPRem]
            return $ testCase (show op) $ do
              let lst =
                    [ conSpec fpNegativeInfinite,
                      conSpec fpPositiveInfinite,
                      conSpec fpNaN,
                      conSpec fpPositiveZero,
                      conSpec fpNegativeZero,
                      conSpec 1,
                      conSpec (-1),
                      symSpec "a",
                      symSpec "b"
                    ]
              let ps =
                    [ fpBinaryOpSpec op l r :: IEEEFPSpec 4 4
                    | l <- lst,
                      r <- lst
                    ]
              traverse_ (validateSpec z3) ps,
          testGroup "RoundingOp" $ do
            let rdgen =
                  elements
                    [ conSpec rna :: FPRoundingModeSpec,
                      conSpec rne,
                      conSpec rtz,
                      conSpec rtp,
                      conSpec rtn
                    ]
            let vgen =
                  elements
                    [ conSpec fpNegativeInfinite :: IEEEFPSpec 4 4,
                      conSpec fpPositiveInfinite,
                      conSpec fpNaN,
                      conSpec fpPositiveZero,
                      conSpec fpNegativeZero,
                      conSpec 120,
                      conSpec 60,
                      conSpec 1,
                      conSpec 2,
                      conSpec 3,
                      conSpec 4,
                      conSpec (-1),
                      conSpec (-2),
                      conSpec (-3),
                      conSpec (-4),
                      conSpec 1.5625e-2,
                      conSpec 2.4e2,
                      conSpec 1.953125e-3,
                      conSpec 1.3671875e-2,
                      symSpec "a",
                      symSpec "b"
                    ]
            [ testGroup "fpRoundingUnaryOp" $ do
                op <- [FPSqrt, FPRoundToIntegral]
                return $
                  testProperty (show op) $
                    forAll rdgen $ \rd ->
                      forAll vgen $ \v ->
                        ioProperty $
                          validateSpec z3 $
                            fpRoundingUnaryOpSpec op rd v,
              testGroup "fpRoundingBinaryOp" $ do
                op <- [FPAdd, FPSub, FPMul, FPDiv]
                return $
                  testProperty (show op) $
                    forAll rdgen $ \rd ->
                      forAll (vectorOf 2 vgen) $ \[l, r] ->
                        ioProperty $
                          validateSpec z3 $
                            fpRoundingBinarySpec op rd l r,
              testProperty "fma" $
                forAll rdgen $ \rd ->
                  forAll (vectorOf 3 vgen) $ \[x, y, z] ->
                    ioProperty $
                      validateSpec z3 $
                        fpFMASpec rd x y z
              ]
        ],
      testGroup "bitCast" $ do
        let bitCastCase ::
              forall a b.
              ( Arbitrary a,
                PEvalBitCastTerm a b,
                SupportedNonFuncPrim a,
                SupportedNonFuncPrim b
              ) =>
              Test
            bitCastCase = testProperty
              (show (typeRep @a) <> " -> " <> show (typeRep @b))
              $ \x ->
                withMaxSuccess 10 . ioProperty $
                  validateSpec
                    z3
                    ( bitCastSpec (conSpec x :: GeneralSpec a) ::
                        GeneralSpec b
                    )
        let fromFPCase ::
              forall a b.
              ( Arbitrary a,
                Arbitrary b,
                PEvalBitCastOrTerm a b,
                RealFloat a,
                SupportedNonFuncPrim a,
                SupportedNonFuncPrim b
              ) =>
              Test
            fromFPCase = testProperty
              (show (typeRep @a) <> " -> " <> show (typeRep @b))
              $ \d x ->
                withMaxSuccess 10 . (not (isNaN x) ==>) . ioProperty $
                  validateSpec
                    z3
                    ( bitCastOrSpec
                        (conSpec d :: GeneralSpec b)
                        (conSpec x :: GeneralSpec a)
                    )
        let toFPCase ::
              forall a b.
              ( Arbitrary a,
                PEvalBitCastTerm a b,
                RealFloat b,
                SupportedNonFuncPrim a,
                SupportedNonFuncPrim b
              ) =>
              Test
            toFPCase = testProperty
              (show (typeRep @a) <> " -> " <> show (typeRep @b))
              $ \x ->
                withMaxSuccess 10
                  . (not (isNaN (bitCast x :: b)) ==>)
                  . ioProperty
                  $ validateSpec
                    z3
                    ( bitCastSpec (conSpec x :: GeneralSpec a) ::
                        GeneralSpec b
                    )
        [ bitCastCase @(IntN 4) @(WordN 4),
          bitCastCase @(WordN 4) @(IntN 4),
          bitCastCase @(IntN 1) @Bool,
          bitCastCase @(WordN 1) @Bool,
          bitCastCase @Bool @(IntN 1),
          bitCastCase @Bool @(WordN 1),
          fromFPCase @(FP 3 5) @(IntN 8),
          fromFPCase @(FP 3 5) @(WordN 8),
          toFPCase @(IntN 8) @(FP 3 5),
          toFPCase @(WordN 8) @(FP 3 5)
          ],
      testGroup "FPConvertible" $ do
        let fromFPAssertion ::
              forall eb sb spec b.
              ( ValidFP eb sb,
                PEvalIEEEFPConvertibleTerm b,
                TermRewritingSpec spec b
              ) =>
              b ->
              FPRoundingMode ->
              FP eb sb ->
              IO ()
            fromFPAssertion d rd x
              | fpIsNaN x = return ()
              | otherwise =
                  validateSpec'
                    z3
                    ( con x
                        .== p
                        .&& symNot (symFpIsNaN p)
                    )
                    ( fromFPOrSpec
                        (conSpec d :: spec)
                        (conSpec rd :: GeneralSpec FPRoundingMode)
                        ( wrap (ssymTerm "p") (conTerm x) ::
                            GeneralSpec (FP eb sb)
                        ) ::
                        spec
                    )
              where
                p = "p" :: SymFP eb sb
            fromFPAssertionDirect ::
              forall eb sb spec b.
              ( ValidFP eb sb,
                PEvalIEEEFPConvertibleTerm b,
                TermRewritingSpec spec b
              ) =>
              b ->
              FPRoundingMode ->
              FP eb sb ->
              IO ()
            fromFPAssertionDirect d rd x =
              validateSpec
                z3
                ( fromFPOrSpec
                    (conSpec d :: spec)
                    (conSpec rd :: GeneralSpec FPRoundingMode)
                    (conSpec x :: GeneralSpec (FP eb sb))
                )
            fromFPCase ::
              forall eb sb spec b.
              ( ValidFP eb sb,
                Arbitrary b,
                PEvalIEEEFPConvertibleTerm b,
                TermRewritingSpec spec b
              ) =>
              Bool ->
              Test
            fromFPCase direct = testProperty
              (show (typeRep @(FP eb sb)) <> " -> " <> show (typeRep @b))
              $ \(d :: b) rd (x :: FP eb sb) ->
                withMaxSuccess 10 . ioProperty $
                  ( if direct
                      then fromFPAssertionDirect @eb @sb @spec
                      else fromFPAssertion @eb @sb @spec
                  )
                    d
                    rd
                    x
            toFPAssertion ::
              forall eb sb b bs.
              ( ValidFP eb sb,
                PEvalIEEEFPConvertibleTerm b,
                LinkedRep b bs,
                Solvable b bs,
                SupportedNonFuncPrim b,
                SymEq bs
              ) =>
              FPRoundingMode ->
              b ->
              IO ()
            toFPAssertion rd x =
              validateSpec'
                z3
                ((con x :: SymType b) .== "p")
                ( toFPSpec
                    (conSpec rd :: GeneralSpec FPRoundingMode)
                    (wrap (ssymTerm "p") (conTerm x) :: GeneralSpec b) ::
                    IEEEFPSpec eb sb
                )
            toFPAssertionFP ::
              forall eb sb eb0 sb0.
              ( ValidFP eb sb,
                ValidFP eb0 sb0
              ) =>
              FPRoundingMode ->
              FP eb0 sb0 ->
              IO ()
            toFPAssertionFP _ x | fpIsNaN x = return ()
            toFPAssertionFP rd x =
              validateSpec'
                z3
                ( con x
                    .== p
                    .&& symNot (symFpIsNaN p)
                )
                ( toFPSpec
                    (conSpec rd :: GeneralSpec FPRoundingMode)
                    ( wrap (ssymTerm "p") (conTerm x) ::
                        GeneralSpec (FP eb0 sb0)
                    ) ::
                    IEEEFPSpec eb sb
                )
              where
                p = "p" :: SymFP eb0 sb0
            toFPCase ::
              forall eb sb b bs.
              ( ValidFP eb sb,
                Arbitrary b,
                PEvalIEEEFPConvertibleTerm b,
                SupportedNonFuncPrim b,
                LinkedRep b bs,
                Solvable b bs,
                SymEq bs
              ) =>
              Test
            toFPCase = testProperty
              (show (typeRep @b) <> " -> " <> show (typeRep @(FP eb sb)))
              $ \rd (x :: b) ->
                withMaxSuccess 10 . ioProperty $
                  toFPAssertion @eb @sb rd x
            toFPCaseFP ::
              forall eb sb eb0 sb0.
              ( ValidFP eb sb,
                ValidFP eb0 sb0
              ) =>
              Test
            toFPCaseFP = testProperty
              ( show (typeRep @(FP eb0 sb0))
                  <> " -> "
                  <> show (typeRep @(FP eb sb))
              )
              $ \rd (x :: b) ->
                withMaxSuccess 10 . ioProperty $
                  toFPAssertionFP @eb @sb @eb0 @sb0 rd x
            specialFps :: (ValidFP eb sb) => [FP eb sb]
            specialFps =
              [ fpPositiveZero,
                fpNegativeZero,
                fpPositiveInfinite,
                fpNegativeInfinite,
                fpNaN,
                fpMaxNormalized,
                fpMinNormalized,
                fpMaxSubnormal,
                fpMinSubnormal
              ]
            boundFps ::
              (ConvertibleBound bv, KnownNat n, 1 <= n, ValidFP eb sb) =>
              bv n ->
              FPRoundingMode ->
              [FP eb sb]
            boundFps n mode =
              [ convertibleLowerBound n mode,
                convertibleUpperBound n mode
              ]
            fps ::
              (ConvertibleBound bv, KnownNat n, 1 <= n, ValidFP eb sb) =>
              bv n ->
              FPRoundingMode ->
              [FP eb sb]
            fps n mode =
              specialFps
                ++ boundFps n mode
                ++ (nextFP <$> boundFps n mode)
                ++ (prevFP <$> boundFps n mode)
            boundedFromFPCase ::
              forall bv n eb sb.
              ( ConvertibleBound bv,
                KnownNat n,
                1 <= n,
                ValidFP eb sb,
                PEvalIEEEFPConvertibleTerm (bv n),
                SupportedNonFuncPrim (bv n),
                Num (bv n),
                Typeable bv
              ) =>
              FPRoundingMode ->
              Test
            boundedFromFPCase mode =
              testCase (show (typeRep @bv) ++ "/" ++ show mode) $
                mapM_
                  ( fromFPAssertion @eb @sb @(GeneralSpec (bv n))
                      123
                      mode
                  )
                  (fps (undefined :: (bv n)) mode)
            boundedFromFPTestGroup ::
              forall n eb sb.
              ( KnownNat n,
                1 <= n,
                ValidFP eb sb
              ) =>
              Test
            boundedFromFPTestGroup =
              testGroup
                ( show (typeRep @(FP eb sb))
                    ++ " -> "
                    ++ show (typeRep @(IntN n))
                    ++ "/"
                    ++ show (typeRep @(WordN n))
                )
                $ do
                  mode <- [rna, rne, rtz, rtp, rtn]
                  [ boundedFromFPCase @IntN @n @eb @sb mode,
                    boundedFromFPCase @WordN @n @eb @sb mode
                    ]
        [ -- z3 is buggy with the indirect encoding
          -- https://github.com/Z3Prover/z3/issues/7321
          fromFPCase @4 @4 @(GeneralSpec AlgReal) True,
          toFPCase @4 @4 @AlgReal,
          testCase "FP 4 4 -> Integer" $ do
            sequence_ $
              (fromFPAssertionDirect @4 @4 @(GeneralSpec Integer) 0)
                <$> [rna, rne, rtz, rtp, rtn]
                <*> (specialFps ++ ((/ 4) . fromIntegral <$> [-7 .. 7])),
          toFPCase @4 @4 @Integer,
          fromFPCase @4 @4 @(IEEEFPSpec 3 3) False,
          toFPCaseFP @4 @4 @3 @3,
          fromFPCase @4 @4 @(IEEEFPSpec 5 5) False,
          toFPCaseFP @4 @4 @5 @5,
          toFPCase @4 @4 @(WordN 8),
          toFPCase @4 @4 @(IntN 8),
          boundedFromFPTestGroup @32 @4 @4,
          boundedFromFPTestGroup @12 @4 @16,
          boundedFromFPTestGroup @12 @4 @12,
          boundedFromFPTestGroup @12 @4 @11,
          boundedFromFPTestGroup @12 @4 @10,
          boundedFromFPTestGroup @12 @4 @9,
          boundedFromFPTestGroup @12 @4 @2,
          boundedFromFPTestGroup @10 @4 @16,
          boundedFromFPTestGroup @10 @4 @10,
          boundedFromFPTestGroup @10 @4 @9,
          boundedFromFPTestGroup @10 @4 @8,
          boundedFromFPTestGroup @10 @4 @7,
          boundedFromFPTestGroup @10 @4 @2,
          boundedFromFPTestGroup @9 @4 @16,
          boundedFromFPTestGroup @9 @4 @9,
          boundedFromFPTestGroup @9 @4 @8,
          boundedFromFPTestGroup @9 @4 @7,
          boundedFromFPTestGroup @9 @4 @6,
          boundedFromFPTestGroup @9 @4 @2,
          boundedFromFPTestGroup @8 @4 @16,
          boundedFromFPTestGroup @8 @4 @8,
          boundedFromFPTestGroup @8 @4 @7,
          boundedFromFPTestGroup @8 @4 @6,
          boundedFromFPTestGroup @8 @4 @5,
          boundedFromFPTestGroup @8 @4 @2,
          boundedFromFPTestGroup @7 @4 @16,
          boundedFromFPTestGroup @7 @4 @7,
          boundedFromFPTestGroup @7 @4 @6,
          boundedFromFPTestGroup @7 @4 @5,
          boundedFromFPTestGroup @7 @4 @4,
          boundedFromFPTestGroup @7 @4 @2,
          boundedFromFPTestGroup @5 @4 @16,
          boundedFromFPTestGroup @5 @4 @5,
          boundedFromFPTestGroup @5 @4 @4,
          boundedFromFPTestGroup @5 @4 @3,
          boundedFromFPTestGroup @5 @4 @2
          ]
    ]
