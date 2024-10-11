{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Test.hs.

{-# LANGUAGE Haskell2010 #-}

module Test.Immutaball.Share.State.Fixtures
	(
		withImmutaball,
		withImmutaball',
		exclusively,
		exclusivelyUnsafeMutex
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Exception

import Control.Concurrent.STM.TMVar
import Control.Lens
import Control.Monad.STM

import qualified Immutaball.Ball.CLI as CLI
import Immutaball.Share.Config
import Immutaball.Share.ImmutaballIO
import Immutaball.Share.State
import Immutaball.Share.Utils

import System.IO.Unsafe (unsafePerformIO)

withImmutaball :: (TMVar a -> IBContext -> Immutaball) -> [String] -> IO a
withImmutaball = withImmutaball' True

withImmutaball' :: Bool -> (TMVar a -> IBContext -> Immutaball) -> [String] -> IO a
withImmutaball' headless immutaball extraArgs = do
	mout <- atomically $ newEmptyTMVar
	let x'cfg = defaultStaticConfig &
		x'cfgInitialWireWithCxt .~ Just (\cxt -> Just $ immutaball mout cxt)
	let args = if' headless ["--headless"] [] ++ extraArgs
	runImmutaballIO $ CLI.immutaballWithArgs x'cfg args
	out <- atomically $ takeTMVar mout
	return out

-- | Tasty provides no feature for mutually exclusive tests, but we only want
-- one SDL instance running at a time.
--
-- We would create a TMVar in a setup IO, but tasty does not seem to provide
-- this feature either unfortunately.
-- So just do it in the static context with unsafePerformIO.
exclusively :: IO a -> IO a
exclusively m = do
	() <- atomically $ takeTMVar exclusivelyUnsafeMutex
	m `finally` do
		atomically $ putTMVar exclusivelyUnsafeMutex ()

{-# NOINLINE exclusivelyUnsafeMutex #-}
exclusivelyUnsafeMutex :: TMVar ()
exclusivelyUnsafeMutex = unsafePerformIO $ newTMVarIO ()
