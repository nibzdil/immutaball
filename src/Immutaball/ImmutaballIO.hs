{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- CLI.hs.

{-# LANGUAGE Haskell2010 #-}

module Immutaball.ImmutaballIO
	(
		ImmutaballIO,
		ImmutaballIOF(..),
		runImmutaballIO
	) where

import System.Exit

import Control.Concurrent.Async

import Immutaball.Utils

type ImmutaballIO = Fixed ImmutaballIOF
data ImmutaballIOF a =
	  DoneImmutaballIOF
	| AndImmutaballIOF a a
	| ExitFailureImmutaballIOF
	| PutStrLn String
	| GetContents (String -> a)

runImmutaballIO :: ImmutaballIO -> IO ()
runImmutaballIO (Fixed (DoneImmutaballIOF))        = return ()
runImmutaballIO (Fixed (AndImmutaballIOF a b))     = a `par` b `par` concurrently_ (runImmutaballIO a) (runImmutaballIO b)
runImmutaballIO (Fixed (ExitFailureImmutaballIOF)) = exitFailure
runImmutaballIO (Fixed (PutStrLn str))             = putStrLn str
runImmutaballIO (Fixed (GetContents withContents)) = getContents >>= runImmutaballIO . withContents
