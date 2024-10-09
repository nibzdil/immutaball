{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- ImmutaballIO.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs, ScopedTypeVariables, ExistentialQuantification #-}

module Immutaball.Share.ImmutaballIO.DirectoryIO
	(
		-- * DirectoryIO
		DirectoryIO,
		DirectoryIOF(..),
		runDirectoryIO,

		-- * mfix
		FixDirectoryIOException(..),
		fixDirectoryIOExceptionToException,
		fixDirectoryIOExceptionFromException,
		PrematureEvaluationFixDirectoryIOException(..),
		EmptyFixDirectoryIOException(..),
		fixDirectoryIOF,
		unsafeFixDirectoryIOFTo,

		-- * Runners
		runDirectoryIOIO,

		-- * DirectoryIO aliases that apply the Fixed wrapper
		mkGetXdgDirectoryData,
		mkGetXdgDirectoryDataSync,
		mkGetXdgDirectoryConfig,
		mkGetXdgDirectoryConfigSync,
		mkGetXdgDirectoryCache,
		mkGetXdgDirectoryCacheSync,
		mkGetXdgDirectoryState,
		mkGetXdgDirectoryStateSync
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Concurrent.Async
import System.Directory

import Immutaball.Share.Utils

-- (mfix imports.)
import Control.Concurrent.MVar
import Control.Exception
import Data.Typeable
import GHC.IO.Unsafe (unsafeDupableInterleaveIO)
import System.IO.Unsafe (unsafePerformIO)

-- * DirectoryIO

type DirectoryIO = Fixed DirectoryIOF
data DirectoryIOF a =
	-- | Get ~/.local/share/path .
	  GetXdgDirectoryData FilePath (Async FilePath -> a)
	| GetXdgDirectoryDataSync FilePath (FilePath -> a)
	-- | Get ~/.local/config/path .
	| GetXdgDirectoryConfig FilePath (Async FilePath -> a)
	| GetXdgDirectoryConfigSync FilePath (FilePath -> a)
	-- | Get ~/.local/cache/path .
	| GetXdgDirectoryCache FilePath (Async FilePath -> a)
	| GetXdgDirectoryCacheSync FilePath (FilePath -> a)
	-- | Get ~/.local/state/path .
	| GetXdgDirectoryState FilePath (Async FilePath -> a)
	| GetXdgDirectoryStateSync FilePath (FilePath -> a)
instance Functor DirectoryIOF where
	fmap :: (a -> b) -> (DirectoryIOF a -> DirectoryIOF b)
	fmap f (GetXdgDirectoryData       path withDir) = GetXdgDirectoryData       path (f . withDir)
	fmap f (GetXdgDirectoryDataSync   path withDir) = GetXdgDirectoryDataSync   path (f . withDir)
	fmap f (GetXdgDirectoryConfig     path withDir) = GetXdgDirectoryConfig     path (f . withDir)
	fmap f (GetXdgDirectoryConfigSync path withDir) = GetXdgDirectoryConfigSync path (f . withDir)
	fmap f (GetXdgDirectoryCache      path withDir) = GetXdgDirectoryCache      path (f . withDir)
	fmap f (GetXdgDirectoryCacheSync  path withDir) = GetXdgDirectoryCacheSync  path (f . withDir)
	fmap f (GetXdgDirectoryState      path withDir) = GetXdgDirectoryState      path (f . withDir)
	fmap f (GetXdgDirectoryStateSync  path withDir) = GetXdgDirectoryStateSync  path (f . withDir)

runDirectoryIO :: DirectoryIO -> IO ()
runDirectoryIO dio = cata runDirectoryIOIO dio

-- TODO: revisit.
{-
instance Foldable DirectoryIOF where
	foldr :: (a -> b -> b) -> b -> DirectoryIOF a -> b
	foldr _reduce reduction0 (GetXdgDirectoryData   _path) = reduction0
	foldr _reduce reduction0 (GetXdgDirectoryConfig _path) = reduction0
	foldr _reduce reduction0 (GetXdgDirectoryCache  _path) = reduction0
	foldr _reduce reduction0 (GetXdgDirectoryState  _path) = reduction0
instance Traversable DirectoryIOF where
	traverse :: Applicative f => (a -> f b) -> DirectoryIOF a -> f (DirectoryIOF b)
	traverse _traversal (GetXdgDirectoryData   path) = pure GetXdgDirectoryData   <*> pure path
	traverse _traversal (GetXdgDirectoryConfig path) = pure GetXdgDirectoryConfig <*> pure path
	traverse _traversal (GetXdgDirectoryCache  path) = pure GetXdgDirectoryCache  <*> pure path
	traverse _traversal (GetXdgDirectoryState  path) = pure GetXdgDirectoryState  <*> pure path
-}

-- * mfix

data FixDirectoryIOException = forall e. Exception e => FixDirectoryIOException e
instance Show FixDirectoryIOException where
	show (FixDirectoryIOException e) = show e
instance Exception FixDirectoryIOException
fixDirectoryIOExceptionToException :: Exception e => e -> SomeException
fixDirectoryIOExceptionToException = toException . FixDirectoryIOException
fixDirectoryIOExceptionFromException :: Exception e => SomeException -> Maybe e
fixDirectoryIOExceptionFromException x = do
	FixDirectoryIOException a <- fromException x
	cast a

data PrematureEvaluationFixDirectoryIOException = PrematureEvaluationFixDirectoryIOException
	deriving (Show)
instance Exception PrematureEvaluationFixDirectoryIOException where
	toException = fixDirectoryIOExceptionToException
	fromException = fixDirectoryIOExceptionFromException

data EmptyFixDirectoryIOException = EmptyFixDirectoryIOException
	deriving (Show)
instance Exception EmptyFixDirectoryIOException where
	toException = fixDirectoryIOExceptionToException
	fromException = fixDirectoryIOExceptionFromException

--    mfix f = mfix f >>= f
-- => mfix f = join $ f <$> mfix f
-- Incorrect: runs f twice.
	--x -> f undefined >>= mfix f
{-
fixDirectoryIOF :: (me -> DirectoryIOF me) -> DirectoryIOF me
fixDirectoryIOF f = case f (error "Error: fixDirectoryIOF: premature evaluation of result before we could start it!") of
	x -> joinDirectoryIOF $ f <$> x
-}
-- Do it like fixIO and fixST (see also their notes; it's a little tricky).
-- Use a lazily read MVar.
fixDirectoryIOF :: (me -> DirectoryIOF me) -> DirectoryIOF me
fixDirectoryIOF f = unsafePerformIO $ do
	mme <- newEmptyMVar
	return $ unsafeFixDirectoryIOFTo mme f

-- | Helper for fixDirectoryIOF.
unsafeFixDirectoryIOFTo :: MVar me -> (me -> DirectoryIOF me) -> DirectoryIOF me
unsafeFixDirectoryIOFTo mme f = unsafePerformIO $ do
	me_ <- unsafeDupableInterleaveIO (readMVar mme `catch` \BlockedIndefinitelyOnMVar -> throwIO PrematureEvaluationFixDirectoryIOException)
	case f me_ of
		_y@(GetXdgDirectoryData       path withDir) -> return $ GetXdgDirectoryData       path ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withDir)
		_y@(GetXdgDirectoryDataSync   path withDir) -> return $ GetXdgDirectoryDataSync   path ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withDir)
		_y@(GetXdgDirectoryConfig     path withDir) -> return $ GetXdgDirectoryConfig     path ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withDir)
		_y@(GetXdgDirectoryConfigSync path withDir) -> return $ GetXdgDirectoryConfigSync path ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withDir)
		_y@(GetXdgDirectoryCache      path withDir) -> return $ GetXdgDirectoryCache      path ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withDir)
		_y@(GetXdgDirectoryCacheSync  path withDir) -> return $ GetXdgDirectoryCacheSync  path ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withDir)
		_y@(GetXdgDirectoryState      path withDir) -> return $ GetXdgDirectoryState      path ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withDir)
		_y@(GetXdgDirectoryStateSync  path withDir) -> return $ GetXdgDirectoryStateSync  path ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withDir)

-- * Runners

runDirectoryIOIO :: DirectoryIOF (IO ()) -> IO ()
runDirectoryIOIO (GetXdgDirectoryData       path withDir) = withAsync (getXdgDirectory XdgData   path) (withDir)
runDirectoryIOIO (GetXdgDirectoryDataSync   path withDir) = getXdgDirectory XdgData   path >>= withDir
runDirectoryIOIO (GetXdgDirectoryConfig     path withDir) = withAsync (getXdgDirectory XdgConfig path) (withDir)
runDirectoryIOIO (GetXdgDirectoryConfigSync path withDir) = getXdgDirectory XdgConfig path >>= withDir
runDirectoryIOIO (GetXdgDirectoryCache      path withDir) = withAsync (getXdgDirectory XdgCache  path) (withDir)
runDirectoryIOIO (GetXdgDirectoryCacheSync  path withDir) = getXdgDirectory XdgCache  path >>= withDir
runDirectoryIOIO (GetXdgDirectoryState      path withDir) = withAsync (getXdgDirectory XdgState  path) (withDir)
runDirectoryIOIO (GetXdgDirectoryStateSync  path withDir) = getXdgDirectory XdgState  path >>= withDir

-- * DirectoryIO aliases that apply the Fixed wrapper

mkGetXdgDirectoryData :: FilePath -> (Async FilePath -> DirectoryIO) -> DirectoryIO
mkGetXdgDirectoryData path withDir = Fixed $ GetXdgDirectoryData path withDir

mkGetXdgDirectoryDataSync :: FilePath -> (FilePath -> DirectoryIO) -> DirectoryIO
mkGetXdgDirectoryDataSync path withDir = Fixed $ GetXdgDirectoryDataSync path withDir

mkGetXdgDirectoryConfig :: FilePath -> (Async FilePath -> DirectoryIO) -> DirectoryIO
mkGetXdgDirectoryConfig path withDir = Fixed $ GetXdgDirectoryConfig path withDir

mkGetXdgDirectoryConfigSync :: FilePath -> (FilePath -> DirectoryIO) -> DirectoryIO
mkGetXdgDirectoryConfigSync path withDir = Fixed $ GetXdgDirectoryConfigSync path withDir

mkGetXdgDirectoryCache :: FilePath -> (Async FilePath -> DirectoryIO) -> DirectoryIO
mkGetXdgDirectoryCache path withDir = Fixed $ GetXdgDirectoryCache path withDir

mkGetXdgDirectoryCacheSync :: FilePath -> (FilePath -> DirectoryIO) -> DirectoryIO
mkGetXdgDirectoryCacheSync path withDir = Fixed $ GetXdgDirectoryCacheSync path withDir

mkGetXdgDirectoryState :: FilePath -> (Async FilePath -> DirectoryIO) -> DirectoryIO
mkGetXdgDirectoryState path withDir = Fixed $ GetXdgDirectoryState path withDir

mkGetXdgDirectoryStateSync :: FilePath -> (FilePath -> DirectoryIO) -> DirectoryIO
mkGetXdgDirectoryStateSync path withDir = Fixed $ GetXdgDirectoryStateSync path withDir
