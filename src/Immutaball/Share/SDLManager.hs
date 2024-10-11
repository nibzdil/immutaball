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
		withSDLManager',
		SDLManagerHandle(..), sdlmh_done, sdlmh_doneReceived, sdlmh_commands,
		SDLManagerCommand(..),
		issueSDLCommand,

		-- * Utils
		sdlGLSwapWindow,
		sdlGL,

		-- * Low level
		initSDLManager,
		quitSDLManager,
		sdlManagerThread
	) where

import Prelude ()
import Immutaball.Prelude

import Data.Functor

import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan
import Control.Lens
import Control.Monad.STM
import qualified SDL.Init
import qualified SDL.Video

import Immutaball.Share.ImmutaballIO
import Immutaball.Share.ImmutaballIO.BasicIO
import Immutaball.Share.ImmutaballIO.GLIO
import Immutaball.Share.ImmutaballIO.SDLIO
import Immutaball.Share.SDLManager.Types
import Immutaball.Share.Utils

-- * High level

-- | Create an SDL manager and manage its lifetime.
withSDLManager :: (SDLManagerHandle -> ImmutaballIO) -> ImmutaballIO
withSDLManager = withSDLManager' False

-- | Adds a headless setting.
withSDLManager' :: Bool -> (SDLManagerHandle -> ImmutaballIO) -> ImmutaballIO
withSDLManager' headless withHandle =
	initSDLManager headless $ \sdlManagerHandle ->
	withHandle sdlManagerHandle <>>
	quitSDLManager sdlManagerHandle

-- Moved to .Types to fix Template Haskell erors:
-- SDLManagerHandle
-- SDLManagerCommand

issueSDLCommand :: SDLManagerHandle -> SDLManagerCommand -> me -> ImmutaballIOF me
issueSDLCommand sdlMgr cmd withUnit = Atomically (writeTChan (sdlMgr^.sdlmh_commands) cmd) (\() -> withUnit)

-- * Utils

-- | Swap the window, but also wait for the swap to finish.
sdlGLSwapWindow :: SDLManagerHandle -> SDL.Video.Window -> me -> ImmutaballIOF me
sdlGLSwapWindow sdlMgr window withUnit =
	JoinIBIOF . JoinIBIOF .
	Atomically (newEmptyTMVar) $ \mdone ->
	issueSDLCommand sdlMgr (GLSwapWindow window mdone) $
	Atomically (takeTMVar mdone) $ \() ->
	withUnit

-- | Run a GLIO in the SDL manager thread.
--
-- This might be needed to avoid issues with multi-threaded SDL & OpenGL on
-- some platforms.
sdlGL :: SDLManagerHandle -> GLIOF me -> ImmutaballIOF me
sdlGL sdlMgr glio =
	JoinIBIOF . JoinIBIOF .
	Atomically (newEmptyTMVar) $ \mme ->
	issueSDLCommand sdlMgr (GLSequence glio mme) $
	Atomically (takeTMVar mme) id

-- * Low level

-- | Manually start the lifetime of the SDLManager OS thread; the caller will
-- need to manage the lifetime.
initSDLManager :: Bool -> (SDLManagerHandle -> ImmutaballIO) -> ImmutaballIO
initSDLManager headless withSdlMgr =
	let initFlags = if' headless [] [SDL.Init.InitVideo, SDL.Init.InitAudio] ++ [SDL.Init.InitJoystick] in
	mkBIO . SDLIO . SDLWithInit initFlags .
	mkBIO . SDLIO . SDLWithTTFInit .
	mkAtomically (newTVar False) $ \done ->
	mkAtomically (newTVar False) $ \doneReceived ->
	mkAtomically newTChan $ \commands ->
	let sdlMgr = SDLManagerHandle {
		_sdlmh_done         = done,
		_sdlmh_doneReceived = doneReceived,
		_sdlmh_commands     = commands
	}
	in mkBIO . ForkOS (sdlManagerThread sdlMgr) $ withSdlMgr sdlMgr

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
			PollEvent to_ -> (mkBIO . SDLIO . SDLPollEventSync $ \mevent -> mkAtomically (writeTMVar to_ mevent) (\() -> mempty)) <>> sdlManagerThread sdlMgr
			PollEvents to_ -> (mkBIO . SDLIO . SDLPollEventsSync $ \events -> mkAtomically (writeTMVar to_ events) (\() -> mempty)) <>> sdlManagerThread sdlMgr
			WithWindow title cfg to_ -> mkBIO . SDLIO . SDLWithWindow title cfg $   \window -> mkAtomically (writeTMVar to_ window) $ \() -> sdlManagerThread sdlMgr
			WithGLContext window to_ -> mkBIO . SDLIO . SDLWithGLContext window $   \cxt    -> mkAtomically (writeTMVar to_ cxt)    $ \() -> sdlManagerThread sdlMgr
			GLSwapWindow window  to_ -> mkBIO . SDLIO . SDLGLSwapWindow window  $              mkAtomically (writeTMVar to_ ())     $ \() -> sdlManagerThread sdlMgr
			GLSequence glio      to_ -> Fixed $ (BasicIBIOF $ GLIO glio)        >>= \me     ->   Atomically (writeTMVar to_ me)     $ \() -> sdlManagerThread sdlMgr
	where
		quit :: ImmutaballIO
		quit = mkAtomically (writeTVar (sdlMgr^.sdlmh_doneReceived) True) mempty <>> mkAtomically (writeTVar (sdlMgr^.sdlmh_done) True) mempty <>> mempty
