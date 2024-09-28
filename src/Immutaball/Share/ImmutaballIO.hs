{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- ImmutaballIO.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs, ScopedTypeVariables #-}

module Immutaball.Share.ImmutaballIO
	(
		-- * ImmutaballIO
		ImmutaballIO,
		ImmutaballIOF(..),
		runImmutaballIO,
		(<>>),

		-- * Runners
		runDirectoryImmutaballIO,

		-- * ImutaballIO aliases that apply the Fixed wrapper
		mkEmptyImmutaballIO,
		mkAndImmutaballIO,
		mkThenImmutaballIO,
		mkExitSuccessImmutaballIO,
		mkExitFailureImmutaballIO,
		mkGetDirectory,
		mkGetArgs,
		mkGetEnvironment,
		mkPutStrLn,
		mkGetContents
	) where

import System.Environment
import System.Exit

import Control.Concurrent.Async
import Control.Parallel

import Immutaball.Share.ImmutaballIO.DirectoryIO
import Immutaball.Share.Utils

-- * ImmutaballIO

type ImmutaballIO = Fixed ImmutaballIOF
data ImmutaballIOF a =
	  EmptyImmutaballIOF
	| AndImmutaballIOF a a
	| ThenImmutaballIOF a a
	| ExitSuccessImmutaballIOF
	| ExitFailureImmutaballIOF

	| GetDirectory (DirectoryIOF a) (FilePath -> a)

	| GetArgs ([String] -> a)
	| GetEnvironment ([(String, String)] -> a)
	| PutStrLn String
	| GetContents (String -> a)

runImmutaballIO :: ImmutaballIO -> IO ()
runImmutaballIO (Fixed (EmptyImmutaballIOF))       = return ()
runImmutaballIO (Fixed (AndImmutaballIOF a b))     = a `par` b `par` concurrently_ (runImmutaballIO a) (runImmutaballIO b)
runImmutaballIO (Fixed (ThenImmutaballIOF a b))    = runImmutaballIO a >> runImmutaballIO b
runImmutaballIO (Fixed (ExitSuccessImmutaballIOF)) = exitSuccess
runImmutaballIO (Fixed (ExitFailureImmutaballIOF)) = exitFailure
runImmutaballIO (Fixed (GetDirectory getDirectory withDirectory)) = runDirectoryIO (runDirectoryAnyIO getDirectory) >>= runImmutaballIO . withDirectory
runImmutaballIO (Fixed (GetArgs withArgs_))        = getArgs >>= runImmutaballIO . withArgs_
runImmutaballIO (Fixed (GetEnvironment withEnvironment)) = getEnvironment >>= runImmutaballIO . withEnvironment
runImmutaballIO (Fixed (PutStrLn str))             = putStrLn str
runImmutaballIO (Fixed (GetContents withContents)) = getContents >>= runImmutaballIO . withContents

instance Semigroup (ImmutaballIOF ImmutaballIO) where
	a <> b = Fixed a `AndImmutaballIOF` Fixed b
instance Monoid (ImmutaballIOF ImmutaballIO) where
	mempty = EmptyImmutaballIOF

instance Semigroup ImmutaballIO where
	(Fixed a) <> (Fixed b) = Fixed (a <> b)
instance Monoid ImmutaballIO where
	mempty = Fixed mempty

instance Functor (ImmutaballIOF) where
	fmap :: (a -> b) -> (ImmutaballIOF a -> ImmutaballIOF b)
	fmap _f (EmptyImmutaballIOF)       = EmptyImmutaballIOF
	fmap  f (AndImmutaballIOF a b)     = AndImmutaballIOF (f a) (f b)
	fmap  f (ThenImmutaballIOF a b)    = ThenImmutaballIOF (f a) (f b)
	fmap _f (ExitFailureImmutaballIOF) = ExitFailureImmutaballIOF
	fmap _f (ExitSuccessImmutaballIOF) = ExitSuccessImmutaballIOF

	fmap  f (GetDirectory getDirectory withDirectory) = GetDirectory (fmap f getDirectory) (f . withDirectory)

	fmap  f (GetArgs withArgs_)              = GetArgs (f . withArgs_)
	fmap  f (GetEnvironment withEnvironment) = GetEnvironment (f . withEnvironment)
	fmap _f (PutStrLn str)                   = PutStrLn str
	fmap  f (GetContents withContents)       = GetContents (f . withContents)

-- | Add an ordering constraint.
infixr 6 <>>
(<>>) :: ImmutaballIO -> ImmutaballIO -> ImmutaballIO
(<>>) = mkThenImmutaballIO

-- * Runners

runDirectoryImmutaballIO :: DirectoryIO -> (FilePath -> ImmutaballIO) -> ImmutaballIO
runDirectoryImmutaballIO (Fixed (GetXdgDirectoryData   path)) withDirectory = Fixed $ GetDirectory (GetXdgDirectoryData   path) withDirectory
runDirectoryImmutaballIO (Fixed (GetXdgDirectoryConfig path)) withDirectory = Fixed $ GetDirectory (GetXdgDirectoryConfig path) withDirectory
runDirectoryImmutaballIO (Fixed (GetXdgDirectoryCache  path)) withDirectory = Fixed $ GetDirectory (GetXdgDirectoryCache  path) withDirectory
runDirectoryImmutaballIO (Fixed (GetXdgDirectoryState  path)) withDirectory = Fixed $ GetDirectory (GetXdgDirectoryState  path) withDirectory

-- * ImutaballIO aliases that apply the Fixed wrapper

mkEmptyImmutaballIO :: ImmutaballIO
mkEmptyImmutaballIO = Fixed $ EmptyImmutaballIOF

mkAndImmutaballIO :: ImmutaballIO -> ImmutaballIO -> ImmutaballIO
mkAndImmutaballIO a b = Fixed $ AndImmutaballIOF a b

mkThenImmutaballIO :: ImmutaballIO -> ImmutaballIO -> ImmutaballIO
mkThenImmutaballIO a b = Fixed $ ThenImmutaballIOF a b

mkExitSuccessImmutaballIO :: ImmutaballIO
mkExitSuccessImmutaballIO = Fixed $ ExitFailureImmutaballIOF

mkExitFailureImmutaballIO :: ImmutaballIO
mkExitFailureImmutaballIO = Fixed $ ExitFailureImmutaballIOF

mkGetDirectory :: DirectoryIOF ImmutaballIO -> (FilePath -> ImmutaballIO) -> ImmutaballIO
mkGetDirectory getDirectory withDirectory = Fixed $ GetDirectory getDirectory withDirectory

mkGetArgs :: ([String] -> ImmutaballIO) -> ImmutaballIO
mkGetArgs withArgs_ = Fixed $ GetArgs withArgs_

mkGetEnvironment :: ([(String, String)] -> ImmutaballIO) -> ImmutaballIO
mkGetEnvironment withEnvironment = Fixed $ GetEnvironment withEnvironment

mkPutStrLn :: String -> ImmutaballIO
mkPutStrLn str = Fixed $ PutStrLn str

mkGetContents :: (String -> ImmutaballIO) -> ImmutaballIO
mkGetContents withContents = Fixed $ GetContents withContents
