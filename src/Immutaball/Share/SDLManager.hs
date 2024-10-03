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

import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan
import Control.Lens
import Control.Monad.STM
import Data.Functor

import Immutaball.Share.ImmutaballIO
import Immutaball.Share.ImmutaballIO.BasicIO
import Immutaball.Share.ImmutaballIO.SDLIO
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
	mkAtomically (newTVar False) $ \done ->
	mkAtomically (newTVar False) $ \doneReceived ->
	mkAtomically newTChan $ \commands ->
	let sdlMgr = SDLManagerHandle {
		_sdlmh_done         = done,
		_sdlmh_doneReceived = doneReceived,
		_sdlmh_commands     = commands
	}
	in (mkBasicImmutaballIO . ForkOS . sdlManagerThread $ sdlMgr) <>> withSdlMgr sdlMgr

-- | Manually close the SDLManager thread low-level.  High-level
-- 'withSDLManager' automatically manages the lifetime.
quitSDLManager :: SDLManagerHandle -> ImmutaballIO
quitSDLManager sdlMgr =
	mkAtomically (do
		writeTVar (sdlMgr^.sdlmh_done) True) (const mempty) <>
	mkAtomically (do
		writeTChan (sdlMgr^.sdlmh_commands) QuitSDLManager) (const mempty) <>>
	mkAtomically (do
		readTVar (sdlMgr^.sdlmh_doneReceived) >>= check) (const mempty)

sdlManagerThread :: SDLManagerHandle -> ImmutaballIO
sdlManagerThread sdlMgr =
	mkAtomically (
		readTVar (sdlMgr^.sdlmh_done) >>= \done ->
		(check done $> Left done) `orElse` (Right <$> readTChan (sdlMgr^.sdlmh_commands))
	) $ \doneOrCmd ->
	case doneOrCmd of
		Left done -> if not done then sdlManagerThread sdlMgr else quit
		Right cmd -> case cmd of
			QuitSDLManager -> quit
			NopSDLManager -> sdlManagerThread sdlMgr
			PollEvent to_ -> (mkBasicImmutaballIO . SDLIO . SDLPollEventSync $ \mevent -> mkAtomically (writeTMVar to_ mevent) (const mempty)) <>> sdlManagerThread sdlMgr
	where
		quit :: ImmutaballIO
		quit = mkAtomically (writeTVar (sdlMgr^.sdlmh_doneReceived) True) mempty <>> mkAtomically (writeTVar (sdlMgr^.sdlmh_done) True) mempty <>> mempty
