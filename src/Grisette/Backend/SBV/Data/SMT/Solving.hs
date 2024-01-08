{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :   Grisette.Backend.SBV.Data.SMT.Solving
-- Copyright   :   (c) Sirui Lu 2021-2023
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Backend.SBV.Data.SMT.Solving
  ( ApproximationConfig (..),
    ExtraConfig (..),
    precise,
    approx,
    withTimeout,
    clearTimeout,
    withApprox,
    clearApprox,
    GrisetteSMTConfig (..),
    SolvingFailure (..),
    TermTy,
    SBVSolverHandle,
    newSBVSolver,
  )
where

import Control.Concurrent.Async (Async (asyncThreadId), async, wait)
import Control.Concurrent.STM
  ( TMVar,
    atomically,
    newTMVarIO,
    putTMVar,
    takeTMVar,
    tryReadTMVar,
  )
import Control.Concurrent.STM.TChan (TChan, newTChan, readTChan, writeTChan)
import Control.Concurrent.STM.TMVar (writeTMVar)
import Control.DeepSeq (NFData)
import Control.Exception (handle, throwTo)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader
  ( MonadReader (ask),
    MonadTrans (lift),
    ReaderT (runReaderT),
  )
import Control.Monad.State (MonadState (get, put), StateT, evalStateT)
import qualified Data.HashSet as S
import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.List (partition)
import Data.Maybe (fromJust)
import qualified Data.SBV as SBV
import qualified Data.SBV.Control as SBVC
import qualified Data.SBV.Trans as SBVT
import qualified Data.SBV.Trans.Control as SBVTC
import GHC.IO.Exception (ExitCode (ExitSuccess))
import GHC.TypeNats (KnownNat, Nat)
import Grisette.Backend.SBV.Data.SMT.Lowering
  ( SymBiMap,
    lowerSinglePrimCached,
    parseModel,
  )
import Grisette.Backend.SBV.Data.SMT.SymBiMap (emptySymBiMap)
import Grisette.Core.Data.BV (IntN, WordN)
import Grisette.Core.Data.Class.CEGISSolver
  ( CEGISCondition (CEGISCondition),
    CEGISSolver (cegisMultiInputs),
  )
import Grisette.Core.Data.Class.EvaluateSym (EvaluateSym (evaluateSym))
import Grisette.Core.Data.Class.ExtractSymbolics
  ( ExtractSymbolics (extractSymbolics),
  )
import Grisette.Core.Data.Class.LogicalOp (LogicalOp (symNot, (.&&)))
import Grisette.Core.Data.Class.ModelOps
  ( ModelOps (exact, exceptFor),
    SymbolSetOps (isEmptySet),
  )
import Grisette.Core.Data.Class.Solvable (Solvable (con))
import Grisette.Core.Data.Class.Solver
  ( IncrementalSolver
      ( solverForceTerminate,
        solverRunCommand,
        solverSolve,
        solverTerminate
      ),
    MonadicIncrementalSolver
      ( monadicSolverPop,
        monadicSolverPush,
        monadicSolverSolve
      ),
    Solver (solve, solveAll, solveMulti),
    SolverCommand (SolverPop, SolverPush, SolverSolve, SolverTerminate),
  )
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.InternedCtors
  ( conTerm,
  )
import Grisette.IR.SymPrim.Data.Prim.InternedTerm.Term
  ( SomeTypedSymbol (SomeTypedSymbol),
    type (-->),
  )
import Grisette.IR.SymPrim.Data.Prim.Model as PM
  ( Model,
    SymbolSet (unSymbolSet),
    equation,
  )
import Grisette.IR.SymPrim.Data.Prim.PartialEval.Bool
  ( pevalNotTerm,
    pevalOrTerm,
  )
import Grisette.IR.SymPrim.Data.SymPrim (SymBool (SymBool))
import Grisette.IR.SymPrim.Data.TabularFun (type (=->))
import Language.Haskell.TH.Syntax (Lift)

-- $setup
-- >>> import Grisette.Core
-- >>> import Grisette.IR.SymPrim
-- >>> import Grisette.Backend.SBV
-- >>> import Data.Proxy

type Aux :: Bool -> Nat -> Type
type family Aux o n where
  Aux 'True _ = SBV.SInteger
  Aux 'False n = SBV.SInt n

type IsZero :: Nat -> Bool
type family IsZero n where
  IsZero 0 = 'True
  IsZero _ = 'False

type TermTy :: Nat -> Type -> Type
type family TermTy bitWidth b where
  TermTy _ Bool = SBV.SBool
  TermTy n Integer = Aux (IsZero n) n
  TermTy _ (IntN x) = SBV.SBV (SBV.IntN x)
  TermTy _ (WordN x) = SBV.SBV (SBV.WordN x)
  TermTy n (a =-> b) = TermTy n a -> TermTy n b
  TermTy n (a --> b) = TermTy n a -> TermTy n b
  TermTy _ v = v

-- | Configures how to approximate unbounded values.
--
-- For example, if we use @'Approx' ('Data.Proxy' :: 'Data.Proxy' 4)@ to approximate the
-- following unbounded integer:
--
-- > (+ a 9)
--
-- We will get
--
-- > (bvadd a #x9)
--
-- Here the value 9 will be approximated to a 4-bit bit vector, and the
-- operation `bvadd` will be used instead of `+`.
--
-- Note that this approximation may not be sound. See 'GrisetteSMTConfig' for
-- more details.
data ApproximationConfig (n :: Nat) where
  NoApprox :: ApproximationConfig 0
  Approx :: (KnownNat n, IsZero n ~ 'False, SBV.BVIsNonZero n) => p n -> ApproximationConfig n

data ExtraConfig (i :: Nat) = ExtraConfig
  { -- | Timeout in milliseconds for each solver call. CEGIS may call the
    -- solver multiple times and each call has its own timeout.
    timeout :: Maybe Int,
    -- | Configures how to approximate unbounded integer values.
    integerApprox :: ApproximationConfig i
  }

preciseExtraConfig :: ExtraConfig 0
preciseExtraConfig =
  ExtraConfig
    { timeout = Nothing,
      integerApprox = NoApprox
    }

approximateExtraConfig ::
  (KnownNat n, IsZero n ~ 'False, SBV.BVIsNonZero n) =>
  p n ->
  ExtraConfig n
approximateExtraConfig p =
  ExtraConfig
    { timeout = Nothing,
      integerApprox = Approx p
    }

-- | Solver configuration for the Grisette SBV backend.
-- A Grisette solver configuration consists of a SBV solver configuration and
-- the reasoning precision.
--
-- Integers can be unbounded (mathematical integer) or bounded (machine
-- integer/bit vector). The two types of integers have their own use cases,
-- and should be used to model different systems.
-- However, the solvers are known to have bad performance on some unbounded
-- integer operations, for example, when reason about non-linear integer
-- algebraic (e.g., multiplication or division),
-- the solver may not be able to get a result in a reasonable time.
-- In contrast, reasoning about bounded integers is usually more efficient.
--
-- To bridge the performance gap between the two types of integers, Grisette
-- allows to model the system with unbounded integers, and evaluate them with
-- infinite precision during the symbolic evaluation, but when solving the
-- queries, they are translated to bit vectors for better performance.
--
-- For example, the Grisette term @5 * "a" :: 'SymInteger'@ should be translated
-- to the following SMT with the unbounded reasoning configuration (the term
-- is @t1@):
--
-- > (declare-fun a () Int)           ; declare symbolic constant a
-- > (define-fun c1 () Int 5)         ; define the concrete value 5
-- > (define-fun t1 () Int (* c1 a))  ; define the term
--
-- While with reasoning precision 4, it would be translated to the following
-- SMT (the term is @t1@):
--
-- > ; declare symbolic constant a, the type is a bit vector with bit width 4
-- > (declare-fun a () (_ BitVec 4))
-- > ; define the concrete value 1, translated to the bit vector #x1
-- > (define-fun c1 () (_ BitVec 4) #x5)
-- > ; define the term, using bit vector addition rather than integer addition
-- > (define-fun t1 () (_ BitVec 4) (bvmul c1 a))
--
-- This bounded translation can usually be solved faster than the unbounded
-- one, and should work well when no overflow is possible, in which case the
-- performance can be improved with almost no cost.
--
-- We must note that the bounded translation is an approximation and is __/not/__
-- __/sound/__. As the approximation happens only during the final translation,
-- the symbolic evaluation may aggressively optimize the term based on the
-- properties of mathematical integer arithmetic. This may cause the solver yield
-- results that is incorrect under both unbounded or bounded semantics.
--
-- The following is an example that is correct under bounded semantics, while is
-- incorrect under the unbounded semantics:
--
-- >>> :set -XTypeApplications -XOverloadedStrings -XDataKinds
-- >>> let a = "a" :: SymInteger
-- >>> solve (precise z3) $ a .> 7 .&& a .< 9
-- Right (Model {a -> 8 :: Integer})
-- >>> solve (approx (Proxy @4) z3) $ a .> 7 .&& a .< 9
-- Left Unsat
--
-- This may be avoided by setting an large enough reasoning precision to prevent
-- overflows.
data GrisetteSMTConfig (i :: Nat) = GrisetteSMTConfig {sbvConfig :: SBV.SMTConfig, extraConfig :: ExtraConfig i}

-- | A precise reasoning configuration with the given SBV solver configuration.
precise :: SBV.SMTConfig -> GrisetteSMTConfig 0
precise config = GrisetteSMTConfig config preciseExtraConfig

-- | An approximate reasoning configuration with the given SBV solver configuration.
approx ::
  forall p n.
  (KnownNat n, IsZero n ~ 'False, SBV.BVIsNonZero n) =>
  p n ->
  SBV.SMTConfig ->
  GrisetteSMTConfig n
approx p config = GrisetteSMTConfig config (approximateExtraConfig p)

-- | Set the timeout for the solver configuration.
withTimeout :: Int -> GrisetteSMTConfig i -> GrisetteSMTConfig i
withTimeout t config = config {extraConfig = (extraConfig config) {timeout = Just t}}

-- | Clear the timeout for the solver configuration.
clearTimeout :: GrisetteSMTConfig i -> GrisetteSMTConfig i
clearTimeout config = config {extraConfig = (extraConfig config) {timeout = Nothing}}

-- | Set the reasoning precision for the solver configuration.
withApprox :: (KnownNat n, IsZero n ~ 'False, SBV.BVIsNonZero n) => p n -> GrisetteSMTConfig i -> GrisetteSMTConfig n
withApprox p config = config {extraConfig = (extraConfig config) {integerApprox = Approx p}}

-- | Clear the reasoning precision and perform precise reasoning with the
-- solver configuration.
clearApprox :: GrisetteSMTConfig i -> GrisetteSMTConfig 0
clearApprox config = config {extraConfig = (extraConfig config) {integerApprox = NoApprox}}

data SolvingFailure
  = DSat (Maybe String)
  | Unsat
  | Unk
  | ResultNumLimitReached
  | SolvingError SBV.SBVException
  | Terminated
  deriving (Show)

sbvCheckSatResult :: SBVC.CheckSatResult -> SolvingFailure
sbvCheckSatResult SBVC.Sat = error "Should not happen"
sbvCheckSatResult (SBVC.DSat msg) = DSat msg
sbvCheckSatResult SBVC.Unsat = Unsat
sbvCheckSatResult SBVC.Unk = Unk

applyTimeout ::
  (MonadIO m, SBVTC.MonadQuery m) => GrisetteSMTConfig i -> m a -> m a
applyTimeout config q = case timeout (extraConfig config) of
  Nothing -> q
  Just t -> SBVTC.timeout t q

instance Solver (GrisetteSMTConfig n) SolvingFailure where
  solve config s = do
    (m, failure) <- solveMulti config 1 s
    case failure of
      ResultNumLimitReached -> return $ Right $ head m
      _ -> return $ Left failure
  solveMulti config n s
    | n > 0 =
        handle
          ( \(x :: SBV.SBVException) -> do
              print "An SBV Exception occurred:"
              print x
              print $
                "Warning: Note that solveMulti do not fully support "
                  ++ "timeouts, and will return an empty list if the solver"
                  ++ "timeouts in any iteration."
              return ([], SolvingError x)
          )
          $ runSBVIncrementalT config
          $ do
            r <- monadicSolverSolve s
            case r of
              Right model -> remainingModels n model
              Left failure -> return ([], failure)
    | otherwise = return ([], ResultNumLimitReached)
    where
      allSymbols = extractSymbolics s :: SymbolSet
      next :: PM.Model -> SBVIncrementalT n IO (Either SolvingFailure PM.Model)
      next md = do
        let newtm =
              S.foldl'
                ( \acc (SomeTypedSymbol _ v) ->
                    pevalOrTerm acc (pevalNotTerm (fromJust $ equation v md))
                )
                (conTerm False)
                (unSymbolSet allSymbols)
        monadicSolverSolve $ SymBool newtm
      remainingModels :: Int -> PM.Model -> SBVIncrementalT n IO ([PM.Model], SolvingFailure)
      remainingModels n1 md
        | n1 > 1 = do
            r <- next md
            case r of
              Left r -> return ([md], r)
              Right mo -> do
                (rmmd, e) <- remainingModels (n1 - 1) mo
                return (md : rmmd, e)
        | otherwise = return ([md], ResultNumLimitReached)
  solveAll = undefined

instance CEGISSolver (GrisetteSMTConfig n) SolvingFailure where
  cegisMultiInputs ::
    forall inputs.
    (ExtractSymbolics inputs, EvaluateSym inputs) =>
    GrisetteSMTConfig n ->
    [inputs] ->
    (inputs -> CEGISCondition) ->
    IO ([inputs], Either SolvingFailure PM.Model)
  cegisMultiInputs config inputs func =
    case symInputs of
      [] -> do
        m <- solve config (cexesAssertFun conInputs)
        return (conInputs, m)
      _ ->
        handle
          ( \(x :: SBV.SBVException) -> do
              print "An SBV Exception occurred:"
              print x
              print $
                "Warning: Note that CEGIS procedures do not fully support "
                  ++ "timeouts, and will return an empty counter example list "
                  ++ "if the solver timeouts during guessing phase."
              return ([], Left $ SolvingError x)
          )
          $ runSBVIncrementalT config
          $ go1
            (cexesAssertFun conInputs)
            conInputs
            (error "Should have at least one gen")
            []
            (con True)
            (con True)
            symInputs
    where
      (conInputs, symInputs) = partition (isEmptySet . extractSymbolics) inputs
      go1 ::
        SymBool ->
        [inputs] ->
        PM.Model ->
        [inputs] ->
        SymBool ->
        SymBool ->
        [inputs] ->
        SBVIncrementalT n IO ([inputs], Either SolvingFailure PM.Model)
      go1 cexFormula cexes previousModel inputs pre post remainingSymInputs = do
        case remainingSymInputs of
          [] -> return (cexes, Right previousModel)
          newInput : vs -> do
            let CEGISCondition nextPre nextPost = func newInput
            let finalPre = pre .&& nextPre
            let finalPost = post .&& nextPost
            r <- go cexFormula newInput (newInput : inputs) finalPre finalPost
            case r of
              (newCexes, Left failure) ->
                return (cexes ++ newCexes, Left failure)
              (newCexes, Right mo) -> do
                go1
                  (cexFormula .&& cexesAssertFun newCexes)
                  (cexes ++ newCexes)
                  mo
                  (newInput : inputs)
                  finalPre
                  finalPost
                  vs
      cexAssertFun input =
        let CEGISCondition pre post = func input in pre .&& post
      cexesAssertFun :: [inputs] -> SymBool
      cexesAssertFun = foldl (\acc x -> acc .&& cexAssertFun x) (con True)
      go ::
        SymBool ->
        inputs ->
        [inputs] ->
        SymBool ->
        SymBool ->
        SBVIncrementalT n IO ([inputs], Either SolvingFailure PM.Model)
      go cexFormula inputs allInputs pre post = do
        r <- monadicSolverSolve $ phi .&& cexFormula
        loop ((forallSymbols `exceptFor`) <$> r) []
        where
          forallSymbols :: SymbolSet
          forallSymbols = extractSymbolics allInputs
          phi = pre .&& post
          negphi = pre .&& symNot post
          check :: Model -> IO (Either SolvingFailure (inputs, PM.Model))
          check candidate = do
            let evaluated = evaluateSym False candidate negphi
            r <- solve config evaluated
            return $ do
              m <- r
              let newm = exact forallSymbols m
              return (evaluateSym False newm inputs, newm)
          guess :: Model -> SBVIncrementalT n IO (Either SolvingFailure PM.Model)
          guess candidate = do
            r <- monadicSolverSolve $ evaluateSym False candidate phi
            return $ exceptFor forallSymbols <$> r
          loop ::
            Either SolvingFailure PM.Model ->
            [inputs] ->
            SBVIncrementalT n IO ([inputs], Either SolvingFailure PM.Model)
          loop (Right mo) cexes = do
            r <- liftIO $ check mo
            case r of
              Left Unsat -> return (cexes, Right mo)
              Left v -> return (cexes, Left v)
              Right (cex, cexm) -> do
                res <- guess cexm
                loop res (cex : cexes)
          loop (Left v) cexes = return (cexes, Left v)

newtype CegisInternal = CegisInternal Int
  deriving (Eq, Show, Ord, Lift)
  deriving newtype (Hashable, NFData)

type SBVIncrementalT n m =
  ReaderT (GrisetteSMTConfig n) (StateT SymBiMap (SBVTC.QueryT m))

type SBVIncremental n = SBVIncrementalT n IO

runSBVIncremental :: GrisetteSMTConfig n -> SBVIncremental n a -> IO a
runSBVIncremental = runSBVIncrementalT

runSBVIncrementalT ::
  (SBVTC.ExtractIO m) =>
  GrisetteSMTConfig n ->
  SBVIncrementalT n m a ->
  m a
runSBVIncrementalT config sbvIncrementalT =
  SBVT.runSMTWith (sbvConfig config) $
    SBVTC.query $
      applyTimeout config $
        flip evalStateT emptySymBiMap $
          runReaderT sbvIncrementalT config

instance
  (MonadIO m) =>
  MonadicIncrementalSolver (SBVIncrementalT n m) SolvingFailure
  where
  monadicSolverSolve (SymBool formula) = do
    symBiMap <- get
    config <- ask
    (newSymBiMap, lowered) <- lowerSinglePrimCached config formula symBiMap
    lift $ lift $ SBV.constrain lowered
    put newSymBiMap
    checkSatResult <- SBVTC.checkSat
    case checkSatResult of
      SBVC.Sat -> do
        sbvModel <- SBVTC.getModel
        let model = parseModel config sbvModel newSymBiMap
        return $ Right model
      r -> return $ Left $ sbvCheckSatResult r
  monadicSolverPush = SBVTC.push
  monadicSolverPop = SBVTC.pop

data SBVSolverStatus = SBVSolverNormal | SBVSolverTerminated

data SBVSolverHandle = SBVSolverHandle
  { sbvSolverHandleMonad :: Async (),
    sbvSolverHandleStatus :: TMVar SBVSolverStatus,
    sbvSolverHandleInChan :: TChan SolverCommand,
    sbvSolverHandleOutChan :: TChan (Either SolvingFailure Model)
  }

newSBVSolver :: GrisetteSMTConfig n -> IO SBVSolverHandle
newSBVSolver config = do
  sbvSolverHandleInChan <- atomically newTChan
  sbvSolverHandleOutChan <- atomically newTChan
  sbvSolverHandleStatus <- newTMVarIO SBVSolverNormal
  sbvSolverHandleMonad <- async $ runSBVIncremental config $ do
    let loop = do
          nextFormula <- liftIO $ atomically $ readTChan sbvSolverHandleInChan
          case nextFormula of
            SolverPush n -> monadicSolverPush n >> loop
            SolverPop n -> monadicSolverPop n >> loop
            SolverTerminate -> return ()
            SolverSolve formula -> do
              r <- monadicSolverSolve formula
              liftIO $ atomically $ writeTChan sbvSolverHandleOutChan r
              loop
    loop
    liftIO $ atomically $ do
      writeTMVar sbvSolverHandleStatus SBVSolverTerminated
      writeTChan sbvSolverHandleOutChan $ Left Terminated
  return $ SBVSolverHandle {..}

instance IncrementalSolver SBVSolverHandle SolvingFailure where
  solverRunCommand f handle@(SBVSolverHandle _ status inChan _) command = do
    st <- liftIO $ atomically $ takeTMVar status
    case st of
      SBVSolverNormal -> do
        liftIO $ atomically $ writeTChan inChan command
        r <- f handle
        liftIO $ atomically $ do
          currStatus <- tryReadTMVar status
          case currStatus of
            Nothing -> putTMVar status SBVSolverNormal
            Just _ -> return ()
        return r
      SBVSolverTerminated -> do
        liftIO $ atomically $ writeTMVar status SBVSolverTerminated
        return $ Left Terminated
  solverSolve handle nextFormula =
    solverRunCommand
      ( \(SBVSolverHandle _ _ _ outChan) ->
          liftIO $ atomically $ readTChan outChan
      )
      handle
      $ SolverSolve nextFormula
  solverTerminate (SBVSolverHandle thread status inChan _) = do
    liftIO $ atomically $ do
      writeTMVar status SBVSolverTerminated
      writeTChan inChan SolverTerminate
    wait thread
  solverForceTerminate (SBVSolverHandle thread status _ outChan) = do
    liftIO $ atomically $ do
      writeTMVar status SBVSolverTerminated
      writeTChan outChan (Left Terminated)
    throwTo (asyncThreadId thread) ExitSuccess
    wait thread
