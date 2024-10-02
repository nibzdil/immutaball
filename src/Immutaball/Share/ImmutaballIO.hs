{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- ImmutaballIO.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs, ScopedTypeVariables, FlexibleContexts, UndecidableInstances #-}

module Immutaball.Share.ImmutaballIO
	(
		-- * ImmutaballIO
		ImmutaballIO,
		ImmutaballIOF(..),
		runImmutaballIO,
		andImmutaballIO,
		thenImmutaballIO,
		(<>>),

		-- * Runners
		runImmutaballIOIO,
		runBasicImmutaballIO,
		runDirectoryImmutaballIO,
		runSDLImmutaballIO,

		-- * ImmutaballIO aliases that apply the Fixed wrapper
		mkEmptyImmutaballIO,
		mkAndImmutaballIO,
		mkThenImmutaballIO,
		mkBasicImmutaballIO
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Concurrent.Async
--import Control.Monad.STM
import Control.Parallel

import Immutaball.Share.ImmutaballIO.BasicIO hiding ((<>>))
import Immutaball.Share.ImmutaballIO.DirectoryIO
import Immutaball.Share.ImmutaballIO.SDLIO
import Immutaball.Share.Utils

-- * ImmutaballIO

type ImmutaballIO = Fixed ImmutaballIOF
data ImmutaballIOF me =
	  EmptyImmutaballIOF
	| AndImmutaballIOF me me
	| ThenImmutaballIOF me me
	| BasicImmutaballIOF (BasicIOF me)

	-- TODO:
	{-
	| Wait (Async in_)
	| WithAsync me (Async in_ -> me)
	| Atomically (STM out)
	-}

runImmutaballIO :: ImmutaballIO -> IO ()
runImmutaballIO bio = cata runImmutaballIOIO bio

andImmutaballIO :: ImmutaballIO -> ImmutaballIO -> ImmutaballIO 
andImmutaballIO x y = Fixed $ AndImmutaballIOF x y

thenImmutaballIO :: ImmutaballIO -> ImmutaballIO -> ImmutaballIO
thenImmutaballIO x y = Fixed $ ThenImmutaballIOF x y

instance Semigroup (ImmutaballIOF ImmutaballIO) where
	(<>) :: ImmutaballIOF ImmutaballIO -> ImmutaballIOF ImmutaballIO -> ImmutaballIOF ImmutaballIO
	a <> b = AndImmutaballIOF (Fixed a) (Fixed b)
instance (Semigroup (ImmutaballIOF me)) => Monoid (ImmutaballIOF me) where
	mempty = EmptyImmutaballIOF

instance Semigroup ImmutaballIO where
	(Fixed a) <> (Fixed b) = Fixed (a <> b)
instance Monoid ImmutaballIO where
	mempty = Fixed EmptyImmutaballIOF

instance Functor ImmutaballIOF where
	fmap :: (a -> b) -> (ImmutaballIOF a -> ImmutaballIOF b)
	fmap _f (EmptyImmutaballIOF)     = EmptyImmutaballIOF
	fmap  f (AndImmutaballIOF a b)   = AndImmutaballIOF (f a) (f b)
	fmap  f (ThenImmutaballIOF a b)  = ThenImmutaballIOF (f a) (f b)
	fmap  f (BasicImmutaballIOF bio) = BasicImmutaballIOF $ f <$> bio

-- | Add an ordering constraint.
infixr 6 <>>
(<>>) :: ImmutaballIO -> ImmutaballIO -> ImmutaballIO
(<>>) = thenImmutaballIO

-- * Runners

runImmutaballIOIO :: ImmutaballIOF (IO ()) -> IO ()
runImmutaballIOIO (EmptyImmutaballIOF)     = return ()
runImmutaballIOIO (AndImmutaballIOF a b)   = a `par` b `par` concurrently_ a b
runImmutaballIOIO (ThenImmutaballIOF a b)  = a >> b
runImmutaballIOIO (BasicImmutaballIOF bio) = runBasicIOIO bio

runBasicImmutaballIO :: BasicIO -> ImmutaballIO
runBasicImmutaballIO bio = Fixed $ BasicImmutaballIOF (runBasicImmutaballIO <$> getFixed bio)

runDirectoryImmutaballIO :: DirectoryIO -> (FilePath -> ImmutaballIO) -> ImmutaballIO
--runDirectoryImmutaballIO dio withPath = runBasicImmutaballIO $ runDirectoryBasicIO dio withPath
runDirectoryImmutaballIO _dio@(Fixed (GetXdgDirectoryData   path)) withPath = Fixed $ BasicImmutaballIOF (GetDirectory (GetXdgDirectoryData   path) withPath)
runDirectoryImmutaballIO _dio@(Fixed (GetXdgDirectoryConfig path)) withPath = Fixed $ BasicImmutaballIOF (GetDirectory (GetXdgDirectoryConfig path) withPath)
runDirectoryImmutaballIO _dio@(Fixed (GetXdgDirectoryCache  path)) withPath = Fixed $ BasicImmutaballIOF (GetDirectory (GetXdgDirectoryCache  path) withPath)
runDirectoryImmutaballIO _dio@(Fixed (GetXdgDirectoryState  path)) withPath = Fixed $ BasicImmutaballIOF (GetDirectory (GetXdgDirectoryState  path) withPath)

runSDLImmutaballIO :: SDLIO -> ImmutaballIO
runSDLImmutaballIO sdlio = runBasicImmutaballIO . runSDLBasicIO $ sdlio

-- * ImutaballIO aliases that apply the Fixed wrapper

mkEmptyImmutaballIO :: ImmutaballIO
mkEmptyImmutaballIO = Fixed $ EmptyImmutaballIOF

mkAndImmutaballIO :: ImmutaballIO -> ImmutaballIO -> ImmutaballIO
mkAndImmutaballIO a b = Fixed $ AndImmutaballIOF a b

mkThenImmutaballIO :: ImmutaballIO -> ImmutaballIO -> ImmutaballIO
mkThenImmutaballIO a b = Fixed $ ThenImmutaballIOF a b

mkBasicImmutaballIO :: BasicIOF ImmutaballIO -> ImmutaballIO
mkBasicImmutaballIO bio = Fixed $ BasicImmutaballIOF bio
