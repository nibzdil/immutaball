{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Test.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Immutaball.Share.State.Fixtures
	(
		withImmutaball,
		withImmutaball',
		withImmutaball'',
		exclusively,
		exclusivelyUnsafeMutex,
		ImmutaballFixture(..), ibfSDLManager,
		unsafeGlobalImmutaballFixture,
		unsafeGlobalImmutaballFixtureInitializerMutex,
		immutaballFixture,
		immutaballFixture',
		initializeImmutaballFixture,
		freeImmutaballFixture
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Exception
import Control.Monad
import Data.Maybe

import Control.Concurrent.STM.TMVar
import Control.Lens
import Control.Monad.STM

import qualified Immutaball.Ball.CLI as CLI
import Immutaball.Share.Config
import Immutaball.Share.ImmutaballIO
import Immutaball.Share.SDLManager
import Immutaball.Share.State
import Immutaball.Share.Utils

import System.IO.Unsafe (unsafePerformIO)

-- ImmutaballFixture moved to fix TH errors.
data ImmutaballFixture = ImmutaballFixture {
	_ibfSDLManager :: Maybe SDLManagerHandle
}
makeLenses ''ImmutaballFixture

withImmutaball :: (TMVar a -> IBContext -> Immutaball) -> [String] -> IO a
withImmutaball = withImmutaball' True

withImmutaball' :: Bool -> (TMVar a -> IBContext -> Immutaball) -> [String] -> IO a
withImmutaball' = withImmutaball'' ["-Q"]

withImmutaball'' :: [String] -> Bool -> (TMVar a -> IBContext -> Immutaball) -> [String] -> IO a
withImmutaball'' baseArgs headless immutaball extraArgs = do
	mout <- atomically $ newEmptyTMVar
	ibf <- immutaballFixture headless
	let x'cfg = defaultStaticConfig &
		x'cfgInitialWireWithCxt .~ Just (\cxt -> Just $ immutaball mout cxt) &
		x'cfgUseExistingSDLManager .~ (ibf^.ibfSDLManager)
	let args = baseArgs ++ if' headless ["--headless"] [] ++ extraArgs
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

-- ImmutaballFixture moved to fix TH errors.

{-# NOINLINE unsafeGlobalImmutaballFixture #-}
unsafeGlobalImmutaballFixture :: TMVar ImmutaballFixture
unsafeGlobalImmutaballFixture = unsafePerformIO $ newEmptyTMVarIO
{-# NOINLINE unsafeGlobalImmutaballFixtureInitializerMutex #-}
unsafeGlobalImmutaballFixtureInitializerMutex :: TMVar ()
unsafeGlobalImmutaballFixtureInitializerMutex = unsafePerformIO $ newTMVarIO ()

immutaballFixture :: Bool -> IO ImmutaballFixture
immutaballFixture = immutaballFixture' True

immutaballFixture' :: Bool -> Bool -> IO ImmutaballFixture
immutaballFixture' sharedSDLManager headless = do
	mibf <- atomically $ do
		() <- takeTMVar unsafeGlobalImmutaballFixtureInitializerMutex
		mibf <- tryReadTMVar unsafeGlobalImmutaballFixture
		when (isJust mibf) $ do
			putTMVar unsafeGlobalImmutaballFixtureInitializerMutex ()
		return mibf
	case mibf of
		Just ibf -> return ibf
		Nothing -> do
			ibf <- initializeImmutaballFixture sharedSDLManager headless
			atomically $ do
				writeTMVar unsafeGlobalImmutaballFixture ibf
				putTMVar unsafeGlobalImmutaballFixtureInitializerMutex ()
			return ibf

initializeImmutaballFixture :: Bool -> Bool -> IO ImmutaballFixture
initializeImmutaballFixture sharedSDLManager headless = do
	mhandle <- atomically $ newEmptyTMVar
	msdlManager <- if' (not sharedSDLManager) (return Nothing) $ do
		runImmutaballIO . initSDLManager headless $ \h -> mkAtomically (putTMVar mhandle h) $ \() -> mkEmptyIBIO
		atomically $ Just <$> takeTMVar mhandle
	return $ ImmutaballFixture {
		_ibfSDLManager = msdlManager
	}

freeImmutaballFixture :: ImmutaballFixture -> IO ()
freeImmutaballFixture ibf = do
	maybe (return ()) (runImmutaballIO . quitSDLManager) (ibf^.ibfSDLManager)
	return ()
