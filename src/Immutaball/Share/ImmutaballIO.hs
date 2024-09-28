{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- ImmutaballIO.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Immutaball.Share.ImmutaballIO
	(
		ImmutaballIO,
		ImmutaballIOF(..),
		runImmutaballIO,

		-- * Aliases that apply the Fixed wrapper
		mkDoneImmutaballIO,
		mkAndImmutaballIO,
		mkExitFailureImmutaballIO,
		mkGetArgs,
		mkGetEnvironment,
		mkPutStrLn,
		mkGetContents
	) where

import System.Environment
import System.Exit

import Control.Concurrent.Async
import Control.Parallel

import Immutaball.Share.Utils

type ImmutaballIO = Fixed ImmutaballIOF
data ImmutaballIOF a =
	  DoneImmutaballIOF
	| AndImmutaballIOF a a
	| ExitFailureImmutaballIOF
	| GetArgs ([String] -> a)
	| GetEnvironment ([(String, String)] -> a)
	| PutStrLn String
	| GetContents (String -> a)

runImmutaballIO :: ImmutaballIO -> IO ()
runImmutaballIO (Fixed (DoneImmutaballIOF))        = return ()
runImmutaballIO (Fixed (AndImmutaballIOF a b))     = a `par` b `par` concurrently_ (runImmutaballIO a) (runImmutaballIO b)
runImmutaballIO (Fixed (ExitFailureImmutaballIOF)) = exitFailure
runImmutaballIO (Fixed (GetArgs withArgs_))        = getArgs >>= runImmutaballIO . withArgs_
runImmutaballIO (Fixed (GetEnvironment withEnvironment)) = getEnvironment >>= runImmutaballIO . withEnvironment
runImmutaballIO (Fixed (PutStrLn str))             = putStrLn str
runImmutaballIO (Fixed (GetContents withContents)) = getContents >>= runImmutaballIO . withContents

instance Semigroup (ImmutaballIOF ImmutaballIO) where
	a <> b = Fixed a `AndImmutaballIOF` Fixed b
instance Monoid (ImmutaballIOF ImmutaballIO) where
	mempty = DoneImmutaballIOF

instance Semigroup ImmutaballIO where
	(Fixed a) <> (Fixed b) = Fixed (a <> b)
instance Monoid ImmutaballIO where
	mempty = Fixed mempty

-- * Aliases that apply the Fixed wrapper

mkDoneImmutaballIO :: ImmutaballIO
mkDoneImmutaballIO = Fixed $ DoneImmutaballIOF

mkAndImmutaballIO :: ImmutaballIO -> ImmutaballIO -> ImmutaballIO
mkAndImmutaballIO a b = Fixed $ AndImmutaballIOF a b

mkExitFailureImmutaballIO :: ImmutaballIO
mkExitFailureImmutaballIO = Fixed $ ExitFailureImmutaballIOF

mkGetArgs :: ([String] -> ImmutaballIO) -> ImmutaballIO
mkGetArgs withArgs_ = Fixed $ GetArgs withArgs_

mkGetEnvironment :: ([(String, String)] -> ImmutaballIO) -> ImmutaballIO
mkGetEnvironment withEnvironment = Fixed $ GetEnvironment withEnvironment

mkPutStrLn :: String -> ImmutaballIO
mkPutStrLn str = Fixed $ PutStrLn str

mkGetContents :: (String -> ImmutaballIO) -> ImmutaballIO
mkGetContents withContents = Fixed $ GetContents withContents
