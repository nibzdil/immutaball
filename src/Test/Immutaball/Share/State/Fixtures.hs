{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Test.hs.

{-# LANGUAGE Haskell2010 #-}

module Test.Immutaball.Share.State.Fixtures
	(
		withImmutaball,
		withImmutaball'
	) where

import Control.Concurrent.STM.TMVar
import Control.Lens
import Control.Monad.STM

import qualified Immutaball.Ball.CLI as CLI
import Immutaball.Share.Config
import Immutaball.Share.Context
import Immutaball.Share.ImmutaballIO
import Immutaball.Share.State
import Immutaball.Share.Utils

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
