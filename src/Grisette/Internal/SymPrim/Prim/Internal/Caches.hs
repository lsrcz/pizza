{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-cse #-}

-- |
-- Module      :   Grisette.Internal.SymPrim.Prim.Internal.Caches
-- Copyright   :   (c) Sirui Lu 2021-2024
-- License     :   BSD-3-Clause (see the LICENSE file)
--
-- Maintainer  :   siruilu@cs.washington.edu
-- Stability   :   Experimental
-- Portability :   GHC only
module Grisette.Internal.SymPrim.Prim.Internal.Caches
  ( SomeStableName (..),
    Id,
    StableIdent,
    Digest,
    CachedInfo (..),
    Interned (..),
    intern,
    haveCache,
    threadCacheSize,
    -- dumpThreadCache,
    threadCacheLiveSize,
  )
where

import Control.Concurrent
  ( MVar,
    ThreadId,
    myThreadId,
    newMVar,
    putMVar,
    takeMVar,
  )
import Control.Monad (replicateM)
import qualified Data.Array as A
import Data.Atomics (atomicModifyIORefCAS, atomicModifyIORefCAS_)
import Data.Data (Proxy (Proxy), Typeable, typeRepFingerprint)
import Data.Foldable (traverse_)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)
import qualified Data.Vector.Unboxed.Mutable as M
import Data.Word (Word32)
import GHC.Base (Any)
import GHC.Fingerprint (Fingerprint)
import GHC.IO (unsafePerformIO)
import GHC.StableName (makeStableName)
import GHC.Weak (Weak, deRefWeak, finalize)
import Grisette.Internal.SymPrim.Prim.Internal.Utils
  ( SomeStableName (SomeStableName),
    WeakThreadId,
    WeakThreadIdRef,
    mkWeakStableNameRefWithFinalizer,
    mkWeakThreadIdRefWithFinalizer,
    myWeakThreadId,
    weakThreadId,
  )
import System.Mem.StableName (StableName)
import Type.Reflection (someTypeRep)
import Unsafe.Coerce (unsafeCoerce)

-- | A unique identifier for a term.
type Id = Word32

-- | The identity of a term.
type StableIdent = StableName Any

-- | A digest of a term.
type Digest = Word32

-- | Information about a cached term.
data CachedInfo = CachedInfo
  { cachedThreadId :: {-# UNPACK #-} !WeakThreadId,
    cachedDigest :: {-# UNPACK #-} !Digest,
    cachedId :: {-# UNPACK #-} !Id,
    cachedStableIdent :: {-# UNPACK #-} !StableIdent
  }

newtype Cache t = Cache {getCache :: A.Array Int (CacheState t)}

type HashTable k v = IORef (HM.HashMap k v)

data CacheState t where
  CacheState ::
    { _sem :: MVar (),
      _nextId :: M.IOVector Id,
      _currentThread :: HashTable (Description t) (Id, Weak StableIdent)
    } ->
    CacheState t

finalizeCacheState :: CacheState t -> IO ()
finalizeCacheState (CacheState _ _ s) = do
  m <- readIORef s
  traverse_ (\(_, w) -> finalize w) m

finalizeCache :: Cache t -> IO ()
finalizeCache (Cache a) = mapM_ finalizeCacheState (A.elems a)

-- | A class for interning terms.
class Interned t where
  data Description t
  type Uninterned t
  describe :: Uninterned t -> Description t
  identify :: CachedInfo -> Uninterned t -> t
  threadId :: t -> WeakThreadId
  descriptionDigest :: Description t -> Digest

{-# NOINLINE termCacheCell #-}
termCacheCell ::
  IORef
    ( HM.HashMap
        WeakThreadId
        ( WeakThreadIdRef,
          IORef (HM.HashMap Fingerprint (Cache Any))
        )
    )
termCacheCell = unsafePerformIO $ newIORef HM.empty

cacheWidth :: Word32
cacheWidth = 10
{-# INLINE cacheWidth #-}

mkCache :: forall t. (Interned t) => IO (Cache t)
mkCache = result
  where
    element =
      CacheState
        <$> newMVar ()
        <*> M.replicate 1 0
        <*> newIORef HM.empty
    result = do
      elements <- replicateM (fromIntegral cacheWidth) element
      return $ Cache $ A.listArray (0, fromIntegral cacheWidth - 1) elements

-- | Internal cache for memoization of term construction. Different types have
-- different caches and they may share names, ids, or representations, but they
-- are not the same term.
typeMemoizedCache ::
  forall a. (Interned a) => ThreadId -> Fingerprint -> IO (Cache a)
typeMemoizedCache tid tyFingerprint = do
  caches <- readIORef termCacheCell
  let wtid = weakThreadId tid
  case HM.lookup wtid caches of
    Just (_, cref) -> do
      cache <- readIORef cref
      case HM.lookup tyFingerprint cache of
        Just d -> return $ unsafeCoerce d
        Nothing -> do
          r1 <- mkCache
          writeIORef cref $!
            HM.insert tyFingerprint (unsafeCoerce r1) cache
          return r1
    Nothing -> do
      r1 <- mkCache
      wtidRef <-
        mkWeakThreadIdRefWithFinalizer tid $ do
          finalizeCache r1
          atomicModifyIORefCAS_ termCacheCell (HM.delete wtid)
      r <- newIORef $ HM.singleton tyFingerprint (unsafeCoerce r1)
      atomicModifyIORefCAS termCacheCell $
        \m -> (HM.insert wtid (wtidRef, r) m, r1)

reclaimTerm ::
  forall t.
  (Interned t, Hashable (Description t), Eq (Description t)) =>
  WeakThreadId ->
  Fingerprint ->
  Int ->
  Description t ->
  IO ()
reclaimTerm id tyFingerprint grp dt = do
  caches <- readIORef termCacheCell
  case HM.lookup id caches of
    Just (_, cref) -> do
      cache <- readIORef cref
      case HM.lookup tyFingerprint cache of
        Just c -> do
          let Cache a = unsafeCoerce c :: Cache t
          let CacheState sem _ s = a A.! grp
          takeMVar sem
          current <- readIORef s
          case HM.lookup dt current of
            Nothing -> return ()
            Just (_, wr) -> do
              t <- deRefWeak wr
              case t of
                Nothing -> writeIORef s $ HM.delete dt current
                Just _ -> return ()
          putMVar sem ()
        Nothing -> return ()
    Nothing -> return ()

-- | Internalize a term.
intern ::
  forall t.
  (Interned t, Typeable t, Hashable (Description t), Eq (Description t)) =>
  Uninterned t ->
  IO t
intern !bt = do
  tid <- myThreadId
  let wtid = weakThreadId tid
  let fingerprint = typeRepFingerprint $ someTypeRep (Proxy @t)
  cache <- typeMemoizedCache tid fingerprint
  let !dt = describe bt :: Description t
      !hdt = descriptionDigest dt
      !r = hdt `mod` cacheWidth
      CacheState sem nextId s = getCache cache A.! (fromIntegral r)
  takeMVar sem
  -- print ("intern", wtid, dt, r)
  current <- readIORef s
  case HM.lookup dt current of
    Nothing -> do
      newId0 <- M.unsafeRead nextId 0
      M.unsafeWrite nextId 0 (newId0 + 1)
      let newId = newId0 * cacheWidth + r
      newIdent <- makeStableName dt
      let anyNewIdent = unsafeCoerce newIdent :: StableIdent
      identRef <-
        mkWeakStableNameRefWithFinalizer anyNewIdent $
          reclaimTerm wtid fingerprint (fromIntegral r) dt
      let !t = identify (CachedInfo (weakThreadId tid) hdt newId anyNewIdent) bt
      writeIORef s $ HM.insert dt (newId, identRef) current
      putMVar sem ()
      return t
    Just (oldId, oldIdentRef) -> do
      t1 <- deRefWeak oldIdentRef
      case t1 of
        Nothing -> do
          newId0 <- M.unsafeRead nextId 0
          M.unsafeWrite nextId 0 (newId0 + 1)
          let newId = newId0 * cacheWidth + r
          newIdent <- makeStableName dt
          let anyNewIdent = unsafeCoerce newIdent :: StableIdent
          identRef <-
            mkWeakStableNameRefWithFinalizer anyNewIdent $
              reclaimTerm wtid fingerprint (fromIntegral r) dt
          let !term =
                identify
                  (CachedInfo (weakThreadId tid) hdt newId anyNewIdent)
                  bt
          writeIORef s $ HM.insert dt (newId, identRef) current
          putMVar sem ()
          return term
        Just t1 -> do
          putMVar sem ()
          return $! identify (CachedInfo (weakThreadId tid) hdt oldId t1) bt
{-# NOINLINE intern #-}

-- | Check if the current thread has a cache.
haveCache :: IO Bool
haveCache = do
  caches <- readIORef termCacheCell
  tid <- myWeakThreadId
  return $ HM.member tid caches

cacheStateSize :: CacheState t -> IO Int
cacheStateSize (CacheState _ _ s) = HM.size <$> readIORef s

cacheStateLiveSize :: CacheState t -> IO Int
cacheStateLiveSize (CacheState sem _ s) = do
  takeMVar sem
  v <- fmap snd . HM.toList <$> readIORef s
  r <-
    sum
      <$> mapM
        ( \(_, x) -> do
            x <- deRefWeak x
            if isJust x then return 1 else return 0
        )
        v
  putMVar sem ()
  return r

{-
dumpCacheState :: CacheState t -> IO ()
dumpCacheState (CacheState sem s) = do
  takeMVar sem
  v <- HM.toList <$> readIORef s
  mapM_
    ( \(k, (i, v)) -> do
        v1 <- deRefWeak v
        case v1 of
          Nothing -> print (k, i, "dead")
          Just r -> print (k, i, r)
    )
    v
  putMVar sem ()

dumpCache :: Cache t -> IO ()
dumpCache (Cache a) = mapM_ dumpCacheState (A.elems a)
-}

cacheSize :: Cache t -> IO Int
cacheSize (Cache a) = sum <$> mapM cacheStateSize (A.elems a)

cacheLiveSize :: Cache t -> IO Int
cacheLiveSize (Cache a) = sum <$> mapM cacheStateLiveSize (A.elems a)

-- | Get the size of the current thread's cache.
threadCacheSize :: WeakThreadId -> IO Int
threadCacheSize tid = do
  caches <- readIORef termCacheCell
  case HM.lookup tid caches of
    Just (_, cref) -> do
      cache <- readIORef cref
      sum <$> mapM cacheSize (HM.elems cache)
    Nothing -> return 0

-- | Get the live size of the current thread's cache.
threadCacheLiveSize :: WeakThreadId -> IO Int
threadCacheLiveSize tid = do
  caches <- readIORef termCacheCell
  case HM.lookup tid caches of
    Just (_, cref) -> do
      cache <- readIORef cref
      sum <$> mapM cacheLiveSize (HM.elems cache)
    Nothing -> return 0

{-
-- | Dump the current thread's cache.
dumpThreadCache :: WeakThreadId -> IO ()
dumpThreadCache tid = do
  caches <- readIORef termCacheCell
  case HM.lookup tid caches of
    Just (_, cref) -> do
      cache <- readIORef cref
      mapM_ dumpCache (HM.elems cache)
    Nothing -> return ()
-}
