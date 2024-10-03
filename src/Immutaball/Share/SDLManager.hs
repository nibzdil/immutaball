{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}

-- | Some SDL functions that some functions can only be called in the OS thread
-- that set up video.
--
-- So I created a module that video setup and input event callers can route
-- their requests through, to ensure they are called from the same OS thread to
-- meet this requirement mentioned in the documentation.
--
-- Example:
-- > pollEvent :: MonadIO m => m (Maybe Event)
-- >
-- > Poll for currently pending events. You can only call this function in the OS thread that set the video mode.
module Immutaball.Share.SDLManager
	(
		-- * High level
		withSDLManager,
		SDLManagerHandle(..), sdlmh_done, sdlmh_doneReceived, sdlmh_commands,
		SDLManagerCommand(..),
		issueCommand,

		-- * Low level
		initSDLManager,
		quitSDLManager,
		sdlManagerThread
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Concurrent.STM.TChan
import Control.Lens

import Immutaball.Share.ImmutaballIO
import Immutaball.Share.ImmutaballIO.BasicIO hiding ((<>>))
import Immutaball.Share.SDLManager.Types

-- * High level

withSDLManager :: (SDLManagerHandle -> ImmutaballIO) -> ImmutaballIO
withSDLManager withHandle =
	initSDLManager $ \sdlManagerHandle ->
	withHandle sdlManagerHandle <>>
	quitSDLManager sdlManagerHandle

-- Moved to .Types to fix Template Haskell erors:
-- SDLManagerHandle
-- SDLManagerCommand

issueCommand :: SDLManagerHandle -> SDLManagerCommand -> ImmutaballIO
issueCommand sdlMgr cmd = mkAtomically (writeTChan (sdlMgr^.sdlmh_commands) cmd) (const mempty)

-- * Low level

-- | Manually start the lifetime of the SDLManager OS thread; the caller will
-- need to manage the lifetime.
initSDLManager :: (SDLManagerHandle -> ImmutaballIO) -> ImmutaballIO
initSDLManager withSdlMgr =
	mkAtomically newTChan $ \done ->
	mkAtomically newTChan $ \doneReceived ->
	mkAtomically newTChan $ \commands ->
	let sdlMgr = SDLManagerHandle {
		_sdlmh_done         = done,
		_sdlmh_doneReceived = doneReceived,
		_sdlmh_commands     = commands
	}
	in (mkBasicImmutaballIO . ForkOS . sdlManagerThread $ sdlMgr) <>> withSdlMgr sdlMgr

-- | Manually close the SDLManager thread low-level.  High-level
-- 'withSDLManager' automatically manages the lifetime.  Currently one quit
-- responds (if this is not desired, switch to TVar bool for doneReceived
-- (TODO: why not do this)).
quitSDLManager :: SDLManagerHandle -> ImmutaballIO
quitSDLManager sdlMgr =
	mkAtomically (do
		writeTChan (sdlMgr^.sdlmh_done) ()) (const mempty) <>
	mkAtomically (do
		writeTChan (sdlMgr^.sdlmh_commands) QuitSDLManager) (const mempty) <>>
	mkAtomically (do
		readTChan (sdlMgr^.sdlmh_doneReceived)) (const mempty)

sdlManagerThread :: SDLManagerHandle -> ImmutaballIO
sdlManagerThread sdlMgr =
	mkAtomically (readTChan $ sdlMgr^.sdlmh_commands) $ \cmd ->
	mkAtomically (tryPeekTChan $ sdlMgr^.sdlmh_done) $ \isDone ->
	case isDone of
		Just () -> quit
		_ -> case cmd of
			QuitSDLManager -> quit
			NopSDLManager -> sdlManagerThread sdlMgr
	where
		quit :: ImmutaballIO
		quit = mkAtomically (writeTChan (sdlMgr^.sdlmh_doneReceived) ()) mempty <>> mempty
