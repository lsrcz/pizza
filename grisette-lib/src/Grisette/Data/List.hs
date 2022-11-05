{-# LANGUAGE FlexibleContexts #-}

module Grisette.Data.List
  ( (!!~),
    symFilter,
    symTake,
    symDrop,
  )
where

import Control.Exception
import Control.Monad.Except
import Grisette.Core.Control.Monad.Union
import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.Error
import Grisette.Core.Data.Class.Integer
import Grisette.Core.Data.Class.Mergeable
import Grisette.Core.Data.Class.SOrd
import Grisette.Core.Data.Class.SimpleMergeable
import Grisette.Lib.Control.Monad

(!!~) ::
  ( SymBoolOp bool,
    GSymIntegerOp bool integer,
    GMonadUnion bool uf,
    MonadError e uf,
    TransformError ArrayException e,
    GMergeable bool a
  ) =>
  [a] ->
  integer ->
  uf a
l !!~ p = go l p 0
  where
    go [] _ _ = throwError $ transformError (IndexOutOfBounds "!!~")
    go (x : xs) p1 i = mrgIf (p1 `gsymeq` i) (mrgReturn x) (go xs p1 $ i + 1)

symFilter :: (SymBoolOp bool, GMonadUnion bool u, GMergeable bool a) => (a -> bool) -> [a] -> u [a]
symFilter f = go
  where
    go [] = mrgReturn []
    go (x : xs) = do
      r <- go xs
      mrgIf (f x) (mrgReturn (x : r)) (mrgReturn r)

symTake :: (SymBoolOp bool, GMonadUnion bool u, GMergeable bool a, GSymIntegerOp bool integer) => integer -> [a] -> u [a]
symTake _ [] = mrgReturn []
symTake x (v : vs) = mrgIf (x `gsymle` 0) (mrgReturn []) (mrgFmap (v :) $ symTake (x - 1) vs)

symDrop :: (SymBoolOp bool, GMonadUnion bool u, GMergeable bool a, GSymIntegerOp bool integer) => integer -> [a] -> u [a]
symDrop _ [] = mrgReturn []
symDrop x r@(_ : vs) = mrgIf (x `gsymle` 0) (mrgReturn r) (symDrop (x - 1) vs)
