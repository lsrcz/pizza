{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Pizza.Core.Data.Class.Solver
  ( Solver (..),
    ExtractUnionEither (..),
    solveFallable,
    solveMultiFallable,
    cegisFallable,
    cegisFallable',
  )
where

import Control.DeepSeq
-- import Control.Monad.Except

-- import Pizza.Core.Control.Monad.UnionMBase

-- import Pizza.Core.Data.Class.GenSym
-- import Pizza.Core.Data.Class.Mergeable

import Control.Monad.Except
import Data.Hashable
import Generics.Deriving
import Language.Haskell.TH.Syntax
import Pizza.Core.Control.Exception
import Pizza.Core.Data.Class.Bool
import Pizza.Core.Data.Class.Evaluate
import Pizza.Core.Data.Class.ExtractSymbolics
import Pizza.Core.Data.Class.PrimWrapper
import Pizza.Core.Data.Class.SimpleMergeable

data SolveInternal = SolveInternal deriving (Eq, Show, Ord, Generic, Hashable, Lift, NFData)

class
  (SymBoolOp bool, EvaluateSym model bool) =>
  Solver config bool symbolSet failure model
    | config -> bool symbolSet failure model
  where
  solveFormula :: config -> bool -> IO (Either failure model)
  solveFormulaMulti :: config -> Int -> bool -> IO [model]
  solveFormulaAll :: config -> Int -> bool -> IO [model]
  cegisFormula ::
    (EvaluateSym model forallArg, ExtractSymbolics symbolSet forallArg) =>
    config ->
    forallArg ->
    bool ->
    IO (Either failure ([forallArg], model))
  cegisFormula config forallArg = cegisFormulas config forallArg (conc False)
  cegisFormulas ::
    (EvaluateSym model forallArg, ExtractSymbolics symbolSet forallArg) =>
    config ->
    forallArg ->
    bool ->
    bool ->
    IO (Either failure ([forallArg], model))

class ExtractUnionEither t u e v | t -> u e v where
  extractUnionEither :: t -> u (Either e v)

instance ExtractUnionEither (ExceptT e u v) u e v where
  extractUnionEither = runExceptT

solveFallable ::
  ( ExtractUnionEither t u e v,
    UnionPrjOp bool u,
    Functor u,
    SymBoolOp bool,
    Solver config bool symbolSet failure model
  ) =>
  config ->
  (Either e v -> bool) ->
  t ->
  IO (Either failure model)
solveFallable config f v = solveFormula config (getSingle $ f <$> extractUnionEither v)

solveMultiFallable ::
  ( ExtractUnionEither t u e v,
    UnionPrjOp bool u,
    Functor u,
    SymBoolOp bool,
    Solver config bool symbolSet failure model
  ) =>
  config ->
  Int ->
  (Either e v -> bool) ->
  t ->
  IO [model]
solveMultiFallable config n f v = solveFormulaMulti config n (getSingle $ f <$> extractUnionEither v)

cegisFallable ::
  ( ExtractUnionEither t u e v,
    UnionPrjOp bool u,
    Functor u,
    SymBoolOp bool,
    EvaluateSym model forallArgs,
    ExtractSymbolics symbolSet forallArgs,
    Solver config bool symbolSet failure model
  ) =>
  config ->
  forallArgs ->
  (Either e v -> (bool, bool)) ->
  t ->
  IO (Either failure ([forallArgs], model))
cegisFallable config args f v = uncurry (cegisFormulas config args) (getSingle $ f <$> extractUnionEither v)

cegisFallable' ::
  ( ExtractUnionEither t u e v,
    UnionPrjOp bool u,
    Monad u,
    SymBoolOp bool,
    EvaluateSym model forallArgs,
    ExtractSymbolics symbolSet forallArgs,
    Solver config bool symbolSet failure model
  ) =>
  config ->
  forallArgs ->
  (Either e v -> u (Either VerificationConditions ())) ->
  t ->
  IO (Either failure ([forallArgs], model))
cegisFallable' config args f v =
  uncurry
    (cegisFormulas config args)
    ( getSingle $
        ( \case
            Left AssumptionViolation -> (conc True, conc False)
            Left AssertionViolation -> (conc False, conc True)
            _ -> (conc False, conc False)
        )
          <$> (extractUnionEither v >>= f)
    )
